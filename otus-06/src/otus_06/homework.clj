(ns otus-06.homework
  (:require
   [clojure.string :as string]))

;; Загрузить данные из трех файлов на диске.
;; Эти данные сформируют вашу базу данных о продажах.
;; Каждая таблица будет иметь «схему», которая указывает поля внутри.
;; Итак, ваша БД будет выглядеть так:

;; cust.txt: это данные для таблицы клиентов. Схема:
;; <custID, name, address, phoneNumber>

;; Примером файла cust.txt может быть:
;; 1|John Smith|123 Here Street|456-4567
;; 2|Sue Jones|43 Rose Court Street|345-7867
;; 3|Fan Yuhong|165 Happy Lane|345-4533

;; Каждое поле разделяется символом «|». и содержит непустую строку.

;; prod.txt: это данные для таблицы продуктов. Схема
;; <prodID, itemDescription, unitCost>

;; Примером файла prod.txt может быть:
;; 1|shoes|14.96
;; 2|milk|1.98
;; 3|jam|2.99
;; 4|gum|1.25
;; 5|eggs|2.98
;; 6|jacket|42.99

;; sales.txt: это данные для основной таблицы продаж. Схема:
;; <salesID, custID, prodID, itemCount>.
;;
;; Примером дискового файла sales.txt может быть:
;; 1|1|1|3
;; 2|2|2|3
;; 3|2|1|1
;; 4|3|3|4

;; Например, первая запись (salesID 1) указывает, что Джон Смит (покупатель 1) купил 3 пары обуви (товар 1).

;; Задача:
;; Предоставить следующее меню, позволяющее пользователю выполнять действия с данными:

;; *** Sales Menu ***
;; ------------------
;; 1. Display Customer Table
;; 2. Display Product Table
;; 3. Display Sales Table
;; 4. Total Sales for Customer
;; 5. Total Count for Product
;; 6. Exit

;; Enter an option?

;; Варианты будут работать следующим образом

;; 1. Вы увидите содержимое таблицы Customer. Вывод должен быть похож (не обязательно идентичен) на

;; 1: ["John Smith" "123 Here Street" "456-4567"]
;; 2: ["Sue Jones" "43 Rose Court Street" "345-7867"]
;; 3: ["Fan Yuhong" "165 Happy Lane" "345-4533"]

;; 2. То же самое для таблицы prod.

;; 3. Таблица продаж немного отличается.
;;    Значения идентификатора не очень полезны для целей просмотра,
;;    поэтому custID следует заменить именем клиента, а prodID — описанием продукта, как показано ниже:
;; 1: ["John Smith" "shoes" "3"]
;; 2: ["Sue Jones" "milk" "3"]
;; 3: ["Sue Jones" "shoes" "1"]
;; 4: ["Fan Yuhong" "jam" "4"]

;; 4. Для варианта 4 вы запросите у пользователя имя клиента.
;;    Затем вы определите общую стоимость покупок для этого клиента.
;;    Итак, для Сью Джонс вы бы отобразили такой результат:
;; Sue Jones: $20.90

;;    Это соответствует 1 паре обуви и 3 пакетам молока.
;;    Если клиент недействителен, вы можете либо указать это в сообщении, либо вернуть $0,00 за результат.

;; 5. Здесь мы делаем то же самое, за исключением того, что мы вычисляем количество продаж для данного продукта.
;;    Итак, для обуви у нас может быть:
;; Shoes: 4

;;    Это представляет три пары для Джона Смита и одну для Сью Джонс.
;;    Опять же, если продукт не найден, вы можете либо сгенерировать сообщение, либо просто вернуть 0.

;; 6. Наконец, если выбрана опция «Выход», программа завершится с сообщением «До свидания».
;;    В противном случае меню будет отображаться снова.

;; *** Дополнительно можно реализовать возможность добавлять новые записи в исходные файлы
;;     Например добавление нового пользователя, добавление новых товаров и новых данных о продажах

;; Файлы находятся в папке otus-06/resources/homework

(defn read-file [filename]
  (->>
   (slurp filename)
   (#(clojure.string/split % #"\n"))
   (map (fn [e] (clojure.string/split e #"\|")))))

(defn get-line-with [lines pos value]
  (some #(when (= (nth % pos) value) %) lines))

(defn get-by-id [filename pos value]
  (let [v (read-file filename)]
    (get-line-with v pos value)))

(defn get-client-total [name]
  (let [client (get-by-id "resources/homework/cust.txt" 1 name)]
    (if client
      (let [client (get-by-id "resources/homework/cust.txt" 1 name)
            client-id (nth  client 0)
            sales (read-file "resources/homework/sales.txt")
            products (read-file "resources/homework/prod.txt")
            client-sales (filter (fn [line] (= (nth line 1) client-id)) sales)
            total (->> client-sales
                       (map (fn [[_ _ prod-id q]]  [q (nth (get-line-with products 0 prod-id) 2)]))
                       (reduce (fn [total [q price]] (+ total (* (Integer/parseInt q) (Float/parseFloat price)))) 0))]

        total) "client not found")))

(defn get-product-count [name]
  (let
   [product-id (->> name
                    (get-by-id "resources/homework/prod.txt" 1)
                    (#(nth %  0)))
    quantity (->>
              (read-file "resources/homework/sales.txt")
              (filter (fn [line] (= (nth line 2) product-id)))
              (map #(Integer/parseInt (nth %  3)))
              (reduce +))]

    quantity))

(defn display-product-count [name]
  (println name (get-product-count name)))

(get-product-count "shoes")

(get-line-with (read-file "resources/homework/prod.txt") 0 "1")

(get-by-id "resources/homework/cust.txt" 0 "1")

(defn read-input []
  (println "")
  (println
   " *** Sales Menu *** \n"
   "------------------ \n"
   "1. Display Customer Table\n"
   "2. Display Product Table\n"
   "3. Display Sales Table\n"
   "4. Total Sales for Customer\n"
   "5. Total Count for Product\n"
   "7. Add new user\n"
   "6. Exit\n"
   "Enter an option?\n")
  (flush)
  (read-line))

(defn display-table [name]
  (println "")
  (println name)
  (println (slurp name))
  (println ""))

(defn display-client-total [name]
  (println name (get-client-total name)))

(defn get-next-id [name]
  (->> name
       (read-file)
       last
       (#(nth % 0))
       (Integer/parseInt)
       inc
       str))

(get-next-id "resources/homework/cust.txt")

(defn ask [what]
  (print (str what ": "))
  (flush)
  (read-line))

(defn add-new-user []
  (let [next-id (get-next-id "resources/homework/cust.txt")
        name (ask "name")
        addr (ask "address")
        phone (ask "phone")
        line (str next-id "|" name "|" addr "|" phone "\n")]
    (spit "resources/homework/cust.txt" line :append true)))

(defn add-new-product []
  (let [next-id (get-next-id "resources/homework/prod.txt")
        name (ask "name")
        price (ask "price")
        line (str next-id "|" name "|" price "\n")]
    (spit "resources/homework/prod.txt" line :append true)))

(defn add-new-sale []
  (let [next-id (get-next-id "resources/homework/sales.txt")
        cust-id (ask "customer id")
        product-id (ask "product id")
        quantity (ask "quantity")
        line (str next-id "|" cust-id "|" product-id "|" quantity "\n")]
    (spit "resources/homework/sales.txt" line :append true)))

(defn -main []
  (loop [line (read-input)]
    (cond
      (= line "1") (do (display-table "resources/homework/cust.txt") (recur (read-input)))
      (= line "2") (do (display-table "resources/homework/prod.txt") (recur (read-input)))
      (= line "3") (do (display-table "resources/homework/sales.txt") (recur (read-input)))
      (= line "4") (do (println "Customer name: ") (display-client-total (read-line)) (recur read-input))
      (= line "5") (do (println "Product name: ") (display-product-count (read-line)) (recur read-input))
      (= line "7") (do (println "New user: ") (add-new-user) (recur read-input))
      (= line "8") (do (println "New product: ") (add-new-product) (recur read-input))
      (= line "9") (do (println "New sale: ") (add-new-sale) (recur read-input))
      (= line "6") (println "Goodbye!")

      :else (recur (read-input)))))

(map (fn [e] (string/split e #"\|")) (string/split (slurp "resources/homework/cust.txt") #"\n"))

(string/split (nth (string/split (slurp "resources/homework/cust.txt") #"\n")  0)  #"\|")

