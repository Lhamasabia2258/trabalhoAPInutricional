;Frontend:(funcionando , sem tradução e com editr dados)

(ns nutri.core
  (:gen-class)
  (:require [clj-http.client :as http]
            [cheshire.core :as json]
            [clojure.string :as str])
  (:import (java.io OutputStreamWriter BufferedWriter)))

(defn extrair-string [v]
  (cond
    (string? v) v
    (sequential? v) (extrair-string (first v))
    :else (str v)))

(defn normalizar-alimento [alimento]
  (update alimento :calorias extrair-string))

(defn ler-double [mensagem]
  (letfn [(tentar []
            (print mensagem) (flush)
            (let [entrada (read-line)]
              (try
                (Double/parseDouble entrada)
                (catch Exception _ 
                  (println "Entrada inválida. Digite um número.")
                  (tentar)))))]
    (tentar)))


;; (def exercicios-pt->en
;;   {"caminhada" "walking"
;;    "corrida" "running"
;;    "natação" "swimming"
;;    "pedalar" "biking"
;;    "bicicleta" "biking"
;;    "flexão" "push-up"
;;    "ski" "ski"
;;    "abdominal" "sit-up"
;;    "pular corda" "jump rope"})

;; (defn traduzir-exercicio [termo]
;;   (get exercicios-pt->en (str/lower-case termo) termo))

;; PERFIL
(defn perfil-existente? []
  (let [resp (http/get "http://localhost:3000/perfil" {:as :json})]
    (not (empty? (:body resp)))))

(defn capturar-dados-perfil []
  (println "Bem-vindo(a) ao sistema de controle nutricional!")
  (print "Digite seu nome: ") (flush)
  (let [nome (read-line)]
    (print "Digite seu sexo (M/F): ") (flush)
    (let [sexo (read-line)
          idade (do (print "Digite sua idade: ") (flush) (read-line))
          altura (ler-double "Digite sua altura (em metros): ")
          peso (ler-double "Digite seu peso (em kg): ")
          perfil {:nome nome :sexo sexo :idade idade :altura altura :peso peso}]
      (http/post "http://localhost:3000/perfil"
                 {:headers {"Content-Type" "application/json"}
                  :body (json/generate-string perfil)})
      (println "\nPerfil salvo com sucesso!\n"))))

(defn ver-dados-perfil []
  (let [response (http/get "http://localhost:3000/perfil" {:as :json})
        perfil (:body response)]
    (println "\n--- Dados do Perfil ---")
    (if (empty? perfil)
      (println "Perfil não preenchido.")
      (do
        (println "Nome: " (:nome perfil))
        (println "Sexo: " (:sexo perfil))
        (println "Idade: " (:idade perfil) "anos")
        (println "Altura: " (:altura perfil) "m")
        (println "Peso: " (:peso perfil) "kg")))))

(defn editar-dados-perfil []
  (println "\n--- Editar Perfil ---")
  (print "Novo nome: ") (flush)
  (let [nome (read-line)]
    (print "Novo sexo (M/F): ") (flush)
    (let [sexo (read-line)
          idade (do (print "Nova idade: ") (flush) (read-line))
          altura (ler-double "Nova altura (em metros): ")
          peso (ler-double "Novo peso (em kg): ")
          perfil {:nome nome :sexo sexo :idade idade :altura altura :peso peso}]
      (http/post "http://localhost:3000/perfil"
                 {:headers {"Content-Type" "application/json"}
                  :body (json/generate-string perfil)})
      (println "\nPerfil atualizado com sucesso!"))))

(defn submenu-dados-perfil []
  (println "\n--- Dados do Perfil ---")
  (println "1. Ver dados")
  (println "2. Editar dados")
  (println "3. Voltar")
  (print "Escolha uma opção: ") (flush)
  (case (read-line)
    "1" (do (ver-dados-perfil) (submenu-dados-perfil))
    "2" (do (editar-dados-perfil) (submenu-dados-perfil))
    "3" nil
    (do (println "Opção inválida.") (submenu-dados-perfil))))


;; ALIMENTOS
(defn buscar-alimentos-remoto [alimento]
  (let [url (str "http://localhost:3000/alimento/" alimento)
        response (http/get url {:as :json :throw-exceptions false})]
    (if (= 200 (:status response))
      (map normalizar-alimento (:body response))
      (do (println "Erro ao buscar alimento.") nil))))

(defn escolher-alimento [alimentos]
  (run! (fn [[idx a]]
          (println (str (inc idx) ". " (:descricao a) " - " (:quantidade a) " - " (:calorias a) " kcal")))
        (map-indexed vector alimentos))
  (print "\nEscolha uma opção: ") (flush)
  (let [escolha (parse-long (read-line))]
    (if (and (>= escolha 1) (<= escolha (count alimentos)))
      (nth alimentos (dec escolha))
      (do (println "Opção inválida.") (escolher-alimento alimentos)))))

(defn adicionar-alimento []
  (print "Digite o nome do alimento: ") (flush)
  (let [nome (read-line)
        alimentos (buscar-alimentos-remoto nome)]
    (if (seq alimentos)
      (let [a (escolher-alimento alimentos)
            qtd (ler-double "Quantas unidades você consumiu? ")
            data (letfn [(valida [] (print "Data (dd/mm/aaaa): ") (flush)
                                  (let [d (read-line)]
                                    (if (or (re-matches #"\d{8}" d)
                                            (re-matches #"\d{2}/\d{2}/\d{4}" d))
                                      d
                                      (do (println "Data inválida.") (recur))))) ]
                   (valida))
            kcal-str (extrair-string (:calorias a))
            kcal (Double/parseDouble (or (re-find #"^\d+" kcal-str) "0"))
            total (* qtd kcal)
            payload {:descricao (:descricao a)
                     :quantidade-consumida qtd
                     :unidade (:quantidade a)
                     :calorias-por-unidade kcal
                     :calorias-total total
                     :data data}]
        (http/post "http://localhost:3000/consumo"
                   {:headers {"Content-Type" "application/json"}
                    :body (json/generate-string payload)})
        (println "\nResumo:")
        (println (:descricao a) "- Total:" total "kcal - Data:" data))
      (println "Nenhum alimento encontrado."))))

;; EXERCÍCIO
(defn adicionar-exercicio []
  (println "Exemplos: Aerobics, archery, badminton, baseball, basketball")
  (print "Digite o nome da atividade (inglês): ") (flush)
  (let [nome (read-line)
        response (http/post "http://localhost:3000/exercicio"
                            {:headers {"Content-Type" "application/json"}
                             :body (json/generate-string {:atividade nome})
                             :as :json
                             :throw-exceptions false})
        corpo (:body response)]
    (if (= 200 (:status response))
      (let [sugestoes (:sugestoes corpo)]
        (println "\nSugestões encontradas:")
        (run! (fn [[idx s]]
                (println (str (inc idx) ". " (:nome s)
                              " | Calorias/hora: " (:calorias_por_hora s))))
              (map-indexed vector sugestoes))
        (print "\nEscolha uma opção: ") (flush)
        (let [opcao (parse-long (read-line))]
          (if (and (>= opcao 1) (<= opcao (count sugestoes)))
            (let [sugestao (nth sugestoes (dec opcao))
      atividade (:nome sugestao)
      calorias-por-hora (:calorias_por_hora sugestao)
      duracao (ler-double "Duração (min): ")
      data (letfn [(valida [] 
                   (print "Data (dd/mm/aaaa): ") (flush)
                   (let [d (read-line)]
                     (if (or (re-matches #"\d{8}" d)
                             (re-matches #"\d{2}/\d{2}/\d{4}" d))
                       d
                       (do (println "Data inválida.") (recur))))) ]
             (valida))
      payload {:atividade atividade 
               :duracao duracao 
               :data data
               :calorias-por-hora calorias-por-hora}
      resp2 (http/post "http://localhost:3000/registrar-exercicio"
                       {:headers {"Content-Type" "application/json"}
                        :body (json/generate-string payload)
                        :as :json
                        :throw-exceptions false})
      corpo2 (:body resp2)]
  (if (= 200 (:status resp2))
    (let [e (:exercicio corpo2)]
      (println "\n--- Resumo do Exercício ---")
      (println "Atividade:" (:atividade e))
      (println "Duração:" (:duracao e) "minutos")
      (println "Calorias gastas:" (:calorias-total e) "kcal")
      (println "Data:" (:data e)))
    (do
      (println "Erro ao registrar exercício.")
      (println "Resposta:" corpo2))))
    (println "Opção inválida."))))
      (println "Nenhuma atividade correspondente encontrada."))))


;; EXTRATO E SALDO
(defn ver-extrato-por-periodo []
  (print "Data inicial (dd/mm/aaaa): ") (flush)
  (let [inicio (read-line)]
    (print "Data final (dd/mm/aaaa): ") (flush)
    (let [fim (read-line)
          response (http/get "http://localhost:3000/extrato"
                             {:query-params {"inicio" inicio "fim" fim}
                              :as :json
                              :throw-exceptions false})]
      (if (= 200 (:status response))
        (do (println "\n--- Extrato ---")
            (run! println (:body response)))
        (println "Erro ao buscar extrato.")))))

(defn ver-saldo-por-periodo []
  (print "Data inicial (dd/mm/aaaa): ") (flush)
  (let [inicio (read-line)]
    (print "Data final (dd/mm/aaaa): ") (flush)
    (let [fim (read-line)
          response (http/get "http://localhost:3000/saldo"
                             {:query-params {"inicio" inicio "fim" fim}
                              :as :json
                              :throw-exceptions false})]
      (if (= 200 (:status response))
        (let [{:keys [ganho perda saldo]} (:body response)]
          (println "\n--- Saldo Calórico ---")
          (println "Ganho: " ganho "kcal")
          (println "Perda: " perda "kcal")
          (println "Saldo: " saldo "kcal"))
        (println "Erro ao calcular saldo.")))))

;; MENU
(defn menu []
  (println "\nMenu:")
  (println "1. Adicionar alimento")
  (println "2. Adicionar exercício")
  (println "3. Dados do perfil")
  (println "4. Ver extrato por período")
  (println "5. Ver saldo calórico por período")
  (println "6. Encerrar")
  (print "Opção: ") (flush)
  (case (read-line)
    "1" (do (adicionar-alimento) (menu))
    "2" (do (adicionar-exercicio) (menu))
    "3" (do (submenu-dados-perfil) (menu))
    "4" (do (ver-extrato-por-periodo) (menu))
    "5" (do (ver-saldo-por-periodo) (menu))
    "6" (println "Encerrando.")
    (do (println "Opção inválida.") (menu))))

;; PONTO DE ENTRADA
(defn -main [& args]
  (binding [*out* (BufferedWriter. (OutputStreamWriter. System/out "UTF-8"))]
    (if (not (perfil-existente?))
      (capturar-dados-perfil))
    (menu)
    (.flush *out*)))