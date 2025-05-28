(ns nutri.core
  (:gen-class)
  (:require [clj-http.client :as http]
            [cheshire.core :as json])
  (:import (java.io OutputStreamWriter BufferedWriter)))

(def historico (atom [])) ;; atom para armazenar o histórico de consumos

(defn buscar-alimentos-remoto [alimento]
  (let [url (str "http://localhost:3000/alimento/" alimento)
        response (http/get url {:as :json :throw-exceptions false})]
    (if (= 200 (:status response))
      (:body response)
      (do
        (println "Erro ao buscar alimento. Status HTTP:" (:status response))
        nil))))

(defn escolher-alimento [alimentos]
  (println "\nEscolha um dos alimentos abaixo digitando o número correspondente:\n")
  (doseq [[i alimento] (map-indexed vector alimentos)]
    (println (str (inc i) ". " (:descricao alimento) " - " (:quantidade alimento) " - " (:calorias alimento) " kcal")))
  (print "\nDigite o número da sua escolha: ")
  (flush)
  (let [escolha (Integer/parseInt (read-line))]
    (if (and (>= escolha 1) (<= escolha (count alimentos)))
      (nth alimentos (dec escolha))
      (do
        (println "Opção inválida. Tente novamente.\n")
        (escolher-alimento alimentos)))))

(defn adicionar-alimento []
  (letfn [(tentar-adicionar []
            (print "Digite o nome do alimento que deseja buscar: ")
            (flush)
            (let [nome (read-line)
                  alimentos (buscar-alimentos-remoto nome)]
              (if (seq alimentos)
                (let [alimento-escolhido (escolher-alimento alimentos)]
                  (print "\nQuantas unidades você consumiu? ")
                  (flush)
                  (let [quantidade (Double/parseDouble (read-line))]
                    (print "Digite a data do consumo (dd/mm/aaaa): ")
                    (flush)
                    (let [data-consumo (read-line)
                          kcal (Double/parseDouble (first (re-find #"\d+(\.\d+)?" (:calorias alimento-escolhido))))
                          total (* quantidade kcal)]
                      ;; Salva no histórico com a data
                      (swap! historico conj {:descricao (:descricao alimento-escolhido)
                                            :quantidade-consumida quantidade
                                            :unidade (:quantidade alimento-escolhido)
                                            :calorias-por-unidade kcal
                                            :calorias-total total
                                            :data data-consumo})
                      (println "\nResumo do consumo:")
                      (println (str "Alimento: " (:descricao alimento-escolhido)))
                      (println (str "Unidade: " (:quantidade alimento-escolhido) " - " kcal " kcal"))
                      (println (str "Você consumiu: " total " kcal"))
                      (println (str "Data do consumo: " data-consumo)))))
                (do
                  (println "\nNenhum alimento encontrado com esse nome. Tente novamente.\n")
                  (recur)))))]
    (tentar-adicionar)))

(defn mostrar-historico []
  (println "\nHistórico de alimentos consumidos:")
  (if (empty? @historico)
    (println "Nenhum alimento consumido até o momento.")
    (doseq [[i registro] (map-indexed vector @historico)]
      (println (format "%d. %s - %.2f %s - Total: %.2f kcal - Data: %s"
                       (inc i)
                       (:descricao registro)
                       (:quantidade-consumida registro)
                       (:unidade registro)
                       (:calorias-total registro)
                       (:data registro))))))

(defn menu []
  (letfn [(menu-recursivo []
            (println "\nO que deseja fazer?")
            (println "1. Adicionar alimento")
            (println "2. Adicionar exercício (em breve)")
            (println "3. Ver histórico")
            (println "4. Encerrar programa")
            (print "Digite o número da opção: ")
            (flush)
            (let [opcao (read-line)]
              (cond
                (= opcao "1") (do (adicionar-alimento) (recur))
                (= opcao "2") (do (println "Função de exercício ainda não implementada.") (recur))
                (= opcao "3") (do (mostrar-historico) (recur))
                (= opcao "4") (println "Encerrando o programa. Até logo!")
                :else (do (println "Opção inválida.") (recur)))))]
    (menu-recursivo)))

(defn -main [& args]
  ;; Redefine *out* para saída em UTF-8
  (binding [*out* (BufferedWriter. (OutputStreamWriter. System/out "UTF-8"))]
    (menu)
    (.flush *out*)))
