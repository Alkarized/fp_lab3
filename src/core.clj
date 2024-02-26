(ns core
  (:require linear)
  (:require lagrange)
  (:require [clojure.string :as str])
  (:require [clojure.tools.cli :refer [parse-opts]]))

(def parser-agent (agent "0")) ;; Агент для преобразования строки в точку
(def worker-agent (agent {:count 0 :points []})) ;; Послыает запросы на линейнуюи нетрполяцию и интерполяцию лагранжа 
(def printer-agent (agent "0")) ;; Общий ввывод текста на стандартный поток вывода
(def lagrange-agent (agent {:num 0 :ans 0})) ;; Агент для интерполяции Лагранжем
(def linear-agent (agent {:num 0 :ans 0})) ;; Агент для линейной интерполяции

(defn print-line [line]
  (send printer-agent (fn [_] line))) ;; Обертка для отправки текста на стандартный поток вывода через Агента

(defn make-struct [n x y] ;; Создание окна, если в окне уже есть N элементов, то новый элемент замещает самый первый.
  (fn [state]
    (let [points (:points state)
          counter (:count state)
          size (count points)]
      (cond
        (> n size)
        (assoc state :count (inc counter) :points (conj points [x y]))

        :else
        (assoc state :count (inc counter) :points (conj (vec (rest points)) [x y]))))))

(defn parser-watch [n] ;; Функция-слушатель для преобразования строки в точку
  (fn [_key _agent _old-state new-state]
    (cond
      (nil? new-state)
      (print-line "all lines procedeed")

      :else
      (let [point (str/split new-state #" ")
            x (Double/parseDouble (first point))
            y (Double/parseDouble (second point))]
        (print-line (str "x: " x ", y: " y))
        (send worker-agent (make-struct n x y))))))

(defn worker-watch [n f b-linear b-lagrange] ;; Функция-слушатель агена послыки запросов интерполяции
  (fn [_key _agent _old-state new-state]
    (let [points (:points new-state)
          size (count (:points new-state))
          is-sized (= n size)
          send-interpolation (fn [ag func] (send ag (fn [state] (assoc state :num (:count new-state) :ans (func points f)))))]

      (print-line (str "i: " (:count new-state) ", (x, y): " (:points new-state)))

      ;;  (when (not is-sized) 
      ;;    (print-line "not enough points in window"))

      (when (and (<= 2 size) b-linear)
        (send-interpolation linear-agent linear/linear-interpolation))

      (when (and is-sized b-lagrange)
        (send-interpolation lagrange-agent lagrange/lagrange-interpolation)))))

(defn printer-watch [] ;; Функция-слушатель для вывода строки
  (fn [_key _agent _old-state new-state]
    (println new-state)))

(defn linear-watch [] ;; Функция-слушатель, которая выводит результаты линейной интерполяции
  (fn [_key _agent _old-state new-state]
    (print-line (str "-----------------\n"
                     "linear values for i: " (:num new-state) "\n"
                     "pairs: " (:ans new-state) "\n"
                     "-----------------"))))

(defn lagrange-watch [] ;; Функция-слушатель, которая выводит результаты интерполяции Лагранжа
  (fn [_key _agent _old-state new-state]
    (print-line (str "-----------------\n"
                     "lagrange values for i: " (:num new-state) "\n"
                     "pairs: " (:ans new-state) "\n"
                     "-----------------"))))

(defn recur-reader [line] ;; Рекурсивное чтение данных с потока ввода
  (send parser-agent (fn [_state] line))
  (cond
    (nil? line)
    (print-line "read all lines ended")

    :else
    (recur (read-line))))

(def cli-options [["-h" "--help" ;; Опции для старта программы
                   :desc "Print help Message"
                   :default false]
                  ["-w" "--window NUMBER"
                   :desc "Set window size"
                   :missing "Window size must be set, look for --help"
                   :parse-fn #(Integer/parseInt %)
                   :validate [#(< 1 %) "Must be an integer number greater than 1"]]
                  ["-f" "--frequency NUMBER"
                   :desc "Set frequency"
                   :missing "Frequency must be set, look for --help"
                   :parse-fn #(Double/parseDouble %)
                   :validate [#(< 0 %) "Must be a dobule number greater than 0"]]
                  ["-i" "--linear"
                   :desc "Use linear interpolation"
                   :default false]
                  ["-a" "--lagrange"
                   :desc "Use lagrange interpolation"
                   :default false]])

(defn -main [& args] ;; Основная функция - общий запуск приложения

  (let [{:keys [options _arguments errors summary]} (parse-opts args cli-options)]

    (when (not (nil? errors))
      (mapv println errors)
      (System/exit 0))

    (when (not (or (:lagrange options) (:linear options)))
      (println "Any of interpolation algorithms must be set, look for --help")
      (System/exit 0))

    (when (:help options)
      (println summary)
      (System/exit 0))

    ;; Накидываем для всех агентов функции-слушатели, которые запускается при изменении значения Агента
    (add-watch parser-agent :watch-key (parser-watch (:window options)))
    (add-watch printer-agent :watch-key (printer-watch))
    (add-watch worker-agent :watch-key (worker-watch (:window options) (:frequency options) (:linear options) (:lagrange options)))
    (add-watch linear-agent :watch-key (linear-watch))
    (add-watch lagrange-agent :watch-key (lagrange-watch))

    (recur-reader (read-line))))
