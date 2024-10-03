(ns otus-06.homework
  (:gen-class)
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

;; Helpers
(defn clear-screen
  []
  (print (str (char 27) "[2J")) ; clear screen
  (print (str (char 27) "[;H")) ; move cursor to the top left corner of the
                                ; screen
)

(defn wait-for-enter
  []
  (let [dot-count (atom 0)
        animation ["/" "-" "\\" "|"]]
    (while (zero? (.available System/in))
      (print (str "\r" "Hit Enter " (nth animation @dot-count) " "))
      (flush)
      (Thread/sleep 1000)
      (swap! dot-count #(if (= % 3) 0 (inc %))))))

;; Patch for (read-line) not working in terminal
;; https://stackoverflow.com/questions/7707558/clojure-read-line-doesnt-wait-for-input
(defn my-read-line [] (nth (line-seq (java.io.BufferedReader. *in*)) 1))


;; Main
(defn base-loader
  "All in memory.
  Instanciates via vec."
  ([file-name schema filter-k filter-v]
   (with-open [f (io/reader (io/resource file-name))]
     (vec (for [line (line-seq f)
                :let [result (zipmap schema (string/split line #"[|]"))]
                :when (if (some? filter-k)
                        (string/includes? (get result filter-k nil) filter-v)
                        true)]
            result)))))

;; Загрузить данные из трех файлов на диске.
;; Эти данные сформируют вашу базу данных о продажах.
;; Каждая таблица будет иметь «схему», которая указывает поля внутри.
;; Итак, ваша БД будет выглядеть так:

;; cust.txt: это данные для таблицы клиентов. Схема:
;; <custID, name, address, phoneNumber>
(def customer-schema [:custID :name :address :phoneNumber])

;; Примером файла cust.txt может быть:
;; 1|John Smith|123 Here Street|456-4567
;; 2|Sue Jones|43 Rose Court Street|345-7867
;; 3|Fan Yuhong|165 Happy Lane|345-4533

;; Каждое поле разделяется символом «|». и содержит непустую строку.
(defn get-customers
  ([] (get-customers nil nil))
  ([filter-k filter-v]
   (base-loader "homework/cust.txt" customer-schema filter-k filter-v)))


(comment
  (get-customers))
; [{"custID" "1",
;   "name" "John Smith",
;   "address" "123 Here Street",
;   "phoneNumber" "456-4567"}
;  {"custID" "2",
;   "name" "Sue Jones",
;   "address" "43 Rose Court Street",
;   "phoneNumber" "345-7867"}
;  {"custID" "3",
;   "name" "Fan Yuhong",
;   "address" "165 Happy Lane",
;   "phoneNumber" "345-4533"}]

;; prod.txt: это данные для таблицы продуктов. Схема
;; <prodID, itemDescription, unitCost>
(def product-schema [:prodID :itemDescription :unitCost])

;; Примером файла prod.txt может быть:
;; 1|shoes|14.96
;; 2|milk|1.98
;; 3|jam|2.99
;; 4|gum|1.25
;; 5|eggs|2.98
;; 6|jacket|42.99
(defn get-products
  ([] (get-products nil nil))
  ([filter-k filter-v]
   (base-loader "homework/prod.txt" product-schema filter-k filter-v)))

(comment
  (get-products))
; [{"prodID" "1", "itemDescription" "shoes", "unitCost" "14.96"}
;  {"prodID" "2", "itemDescription" "milk", "unitCost" "1.98"}
;  {"prodID" "3", "itemDescription" "jam", "unitCost" "2.99"}
;  {"prodID" "4", "itemDescription" "gum", "unitCost" "1.25"}
;  {"prodID" "5", "itemDescription" "eggs", "unitCost" "2.98"}
;  {"prodID" "6", "itemDescription" "jacket", "unitCost" "42.99"}]

;; sales.txt: это данные для основной таблицы продаж. Схема:
;; <salesID, custID, prodID, itemCount>.
(def sales-schema [:salesID :custID :prodID :itemCount])
;;
;; Примером дискового файла sales.txt может быть:
;; 1|1|1|3
;; 2|2|2|3
;; 3|2|1|1
;; 4|3|3|4

(defn get-sales
  ([] (get-sales nil nil))
  ([filter-k filter-v]
   (base-loader "homework/sales.txt" sales-schema filter-k filter-v)))

(comment
  (get-sales))
; [{"salesID" "1", "custID" "1", "prodID" "1", "itemCount" "3"}
;  {"salesID" "2", "custID" "2", "prodID" "2", "itemCount" "3"}
;  {"salesID" "3", "custID" "2", "prodID" "1", "itemCount" "1"}
;  {"salesID" "4", "custID" "3", "prodID" "3", "itemCount" "4"}]

;; Например, первая запись (salesID 1) указывает, что Джон Смит (покупатель 1)
;; купил 3 пары обуви (товар 1).

;; Задача:
;; Предоставить следующее меню, позволяющее пользователю выполнять действия с
;; данными:

;; *** Sales Menu ***
;; ------------------

;; 1. Display Customer Table
(defn display-customers
  []
  (doseq [line (for [customer (get-customers)
                     :let [{:keys [name address phoneNumber]} customer]]
                 (format "Name: %s, Address: %s, Phone: %s"
                         name
                         address
                         phoneNumber))]
    (println line)))

;; 2. Display Product Table
(defn display-products
  []
  (doseq [line (for [product (get-products)
                     :let [{:keys [itemDescription unitCost]} product]]
                 (format "Name: %s, Cost: $%s" itemDescription unitCost))]
    (println line)))

;; 3. Display Sales Table
(defn display-sales
  []
  (let [customers (into {}
                        (for [customer (get-customers)
                              :let [{:keys [custID name]} customer]]
                          [custID name]))
        products (into {}
                       (for [product (get-products)
                             :let [{:keys [prodID itemDescription]} product]]
                         [prodID itemDescription]))]
    (doseq [line (for [sales (get-sales)
                       :let [{:keys [custID prodID itemCount]} sales
                             client (get customers custID)
                             product (get products prodID)]]
                   (format "Client: %s, Product: %s, Count: %s"
                           client
                           product
                           itemCount))]
      (println line))))

;; 4. Total Sales for Customer
(defn show-sales-customer
  "Supports showing result for multiple customers."
  []
  (print "Enter customer name: ")
  (flush)
  (let [name (my-read-line)
        product-prices (into {}
                             (for [product (get-products)
                                   :let [{:keys [prodID unitCost]} product]]
                               [prodID unitCost]))
        sales (apply merge-with
                +
                (for [customer (get-customers :name name)
                      :let [{:keys [custID name]} customer]
                      sales (get-sales :custID custID)
                      :let [{:keys [itemCount prodID]} sales
                            sum (* (Double. itemCount)
                                   (Double. (get product-prices prodID)))]]
                  {name sum}))
        sales-with-default (if (seq sales) sales {"Not found" 0})]
    (doseq [line sales-with-default]
      (println (format "Name: %s, Sum: $%s" (key line) (val line))))))

;; 5. Total Count for Product

(defn show-sales-product
  []
  (print "Enter product name: ")
  (flush)
  (let [item-desc (my-read-line)
        sales (apply merge-with
                +
                (for [product (get-products :itemDescription item-desc)
                      :let [{:keys [prodID itemDescription]} product]
                      sales (get-sales :prodID prodID)
                      :let [{:keys [itemCount]} sales]]
                  {itemDescription (Double. itemCount)}))
        sales-with-default (if (seq sales) sales {"Not found" 0})]
    (doseq [line sales-with-default]
      (println (format "Product: %s, Count sold: %s" (key line) (val line))))))
;; 6. Exit
(defn exit [] (println "Bye!") (System/exit 0))

;; Enter an option?

(defn menu
  []
  (clear-screen)
  (doseq [menu ["1. Display Customer Table" "2. Display Product Table"
                "3. Display Sales Table" "4. Total Sales for Customer"
                "5. Total Count for Product" "6. Exit"]]
    (println menu))
  (flush)
  (read))

;; Варианты будут работать следующим образом

;; 1. Вы увидите содержимое таблицы Customer. Вывод должен быть похож (не
;; обязательно идентичен) на

;; 1: ["John Smith" "123 Here Street" "456-4567"]
;; 2: ["Sue Jones" "43 Rose Court Street" "345-7867"]
;; 3: ["Fan Yuhong" "165 Happy Lane" "345-4533"]

;; 2. То же самое для таблицы prod.

;; 3. Таблица продаж немного отличается.
;;    Значения идентификатора не очень полезны для целей просмотра,
;;    поэтому custID следует заменить именем клиента, а prodID — описанием
;;    продукта, как показано ниже:
;; 1: ["John Smith" "shoes" "3"]
;; 2: ["Sue Jones" "milk" "3"]
;; 3: ["Sue Jones" "shoes" "1"]
;; 4: ["Fan Yuhong" "jam" "4"]

;; 4. Для варианта 4 вы запросите у пользователя имя клиента.
;;    Затем вы определите общую стоимость покупок для этого клиента.
;;    Итак, для Сью Джонс вы бы отобразили такой результат:
;; Sue Jones: $20.90

;;    Это соответствует 1 паре обуви и 3 пакетам молока.
;;    Если клиент недействителен, вы можете либо указать это в сообщении, либо
;;    вернуть $0,00 за результат.

;; 5. Здесь мы делаем то же самое, за исключением того, что мы вычисляем
;; количество продаж для данного продукта.
;;    Итак, для обуви у нас может быть:
;; Shoes: 4

;;    Это представляет три пары для Джона Смита и одну для Сью Джонс.
;;    Опять же, если продукт не найден, вы можете либо сгенерировать сообщение,
;;    либо просто вернуть 0.

;; 6. Наконец, если выбрана опция «Выход», программа завершится с сообщением
;; «До
;; свидания».
;;    В противном случае меню будет отображаться снова.


;; *** Дополнительно можно реализовать возможность добавлять новые записи в
;; исходные файлы
;;     Например добавление нового пользователя, добавление новых товаров и
;;     новых
;;     данных о продажах


;; Файлы находятся в папке otus-06/resources/homework

(defn menu-item [func] (clear-screen) (func) (wait-for-enter))

(defn -main
  [& args]
  (while true
    (condp = (menu)
      1 (menu-item display-customers)
      2 (menu-item display-products)
      3 (menu-item display-sales)
      4 (menu-item show-sales-customer)
      5 (menu-item show-sales-product)
      6 (exit)
      (do (println "Error: Non existing menu item.\n") (wait-for-enter)))))
