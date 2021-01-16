;; *********** Шаблоны ***********
(deftemplate company "Структура Компания"
	(slot position (type INTEGER)) ;; позиция в итоговом рейтинге компаний, которые лучше всего удовлетворяют требованиям клиента
	(multislot company_name)
	(slot price) 
	(multislot cable_type)
	(slot amount (type INTEGER))
	(multislot quality)
	(slot phone)
	(multislot services)
	(slot points (type FLOAT))
)
(deftemplate buyer "Структура Покупатель"
	(slot selection_by) ;; требование, по которому будут отбираться компании
	(slot price)
	(multislot services)
	(slot amount (type INTEGER))
	(multislot quality)
)

;; *********** Начальные факты ***********
(deffacts start-facts ""
	(buyer
		(selection_by services)
		(price medium)
		(services TV internet landline_phone)
		(amount 40)
		(quality FullHD)
	)
)

;; *********** Шаблоны для активации правил ***********
(deftemplate Check-MainSelection "Компании, которые подлежат проверке на соответсвие главному требованию"
	(multislot company_name)
)

(deftemplate Collision 
	(multislot company_name1)
	(multislot company_name2)
)

(deftemplate Check
	(multislot company_name)
)

(deftemplate Check-OtherSelection
	(multislot company_name)
)

(deftemplate Add-Point
	(multislot company_name)
	(slot points)
)

(deftemplate Check-Price
	(multislot company_name)
	(slot mainSelection)
	
)

(deftemplate Check-Services
	(multislot company_name)
	(slot mainSelection)
)

(deftemplate Check-Amount
	(multislot company_name)
	(slot mainSelection)
)

(deftemplate Check-Quality
	(multislot company_name)
	(slot mainSelection)
)

;; *********** Правила ***********

(defrule Clear-Output-File "Очистка выходного файла"
	(declare (salience 100))
	=>
	(open "Results.txt" Out "w")
(close Out))

(defrule ReadCompanies "Загрузка поставщиков услуг кабельного ТВ"
	(declare(salience 10))
	(test(open "Companies.txt" Companies "r"))
	=>
	(bind ?line "")
	(bind ?company_name 0)
	(bind ?price 0)
	(bind ?services 0)
	(bind ?cable_type 0)
	(bind ?amount 0)
	(bind ?quality 0)
	(bind ?phone 0)
	(bind ?position 1)
	(bind ?points 0)
	
	(while(not(eq ?line EOF)) do
		(bind ?line (readline Companies))
		(if (eq ?line EOF) then (break))
		(switch (implode$ (first$ (explode$ ?line)))
			(case "Company_name:" then
				(bind ?company_name (rest$(explode$ ?line))))
			(case "Price:" then
				(bind ?price (rest$(explode$ ?line))))
			(case "Cable_type:" then
				(bind ?cable_type (rest$(explode$ ?line))))
			(case "Amount:" then
				(bind ?amount (rest$(explode$ ?line))))
			(case "Quality:" then
				(bind ?quality (rest$(explode$ ?line))))
			(case "Phone:" then
				(bind ?phone (rest$(explode$ ?line))))
			(case "Services:" then
				(bind ?services (rest$(explode$ ?line))))
		)
		(if (not(eq ?services 0)) then
			(assert 
				(company
					(company_name ?company_name)
					(price (nth$ 1 ?price))
					(services ?services)
					(cable_type (nth$ 1 ?cable_type))
					(amount (integer (nth$ 1 ?amount)))
					(quality (nth$ 1 ?quality))
					(phone (nth$ 1 ?phone))
					(position ?position)
					(points ?points)
				)
			)
			(assert (Check-MainSelection(company_name ?company_name)))
			(bind ?position (+ ?position 1))
			(bind ?services 0)
		)	
		
	)		
	(close Companies)
)

(defrule CheckMainSelection "Проверка команий на главные требования"
	(declare (salience 9))
	
	(buyer (selection_by ?sel_by))
	?f1<-(Check-MainSelection (company_name $?co_name))
	?company<-(company 	(company_name $?co_name))
	=>
	(retract ?f1)
	
	(switch ?sel_by
		(case price then
			(assert(Check-Price(company_name $?co_name)(mainSelection ?sel_by)))
		)	
		(case services then
			(assert(Check-Services(company_name $?co_name)(mainSelection ?sel_by)))
		)
		(case amount then
			(assert(Check-Amount(company_name $?co_name)(mainSelection ?sel_by)))
		)
		(case quality then
			(assert(Check-Quality(company_name $?co_name)(mainSelection ?sel_by)))
		)
	)
	(assert (Check-OtherSelection (company_name $?co_name)))
)	

(defrule AddPoint "Добавление баллов за совпадение по требованям"
	(declare (salience 8))
	?f1<-(Add-Point(company_name $?co_name) (points ?addpts))
	?company<-(company(company_name $?co_name) (points ?co_pts))
	=>
	(retract ?f1)
	(modify ?company (points (+ ?co_pts ?addpts)))
	
)

(defrule CheckOtherSelection "Проверка на совпадение остальных требований"
	(declare (salience 6))
	(buyer 
		(price ?price)
		(services $?cl_serv) 
		(amount ?cl_amount) 
		(quality $?cl_quality)
	)
	?f1<-(Check-OtherSelection(company_name $?co_name))
	?company1<-(company 
				(company_name $?co_name)
				(price ?co_price)
				(services $?co_serv)
				(amount ?co_amount)
				(quality $?co_quality)
			)
	=>
	(retract ?f1)
	
	(assert (Check-Price(company_name $?co_name)))
	(assert (Check-Services(company_name $?co_name)))
	(assert (Check-Amount(company_name $?co_name)))
	(assert (Check-Quality(company_name $?co_name)))
	
)

(defrule CheckPrice
	(declare (salience 7))
	(buyer (price ?cl_price))
	?f1<-(Check-Price (company_name $?co_name)(mainSelection ?sel_by))
	?company<-(company(company_name $?co_name)(price ?co_price))
	=>
	(retract ?f1)
	(if (= (length$ ?co_price)(length$ ?cl_price)) then
		(if(eq ?sel_by price) then
			(assert (Add-Point (company_name $?co_name)(points 1)))
			(printout t "GENERAL" crlf)
		else
			(assert (Add-Point (company_name $?co_name)(points 0.2)))
		)
	)
	
)

(defrule CheckServices
	(declare (salience 6))
	(buyer (services $?cl_serv))
	?f1<-(Check-Services (company_name $?co_name)(mainSelection ?sel_by))
	?company<-(company(company_name $?co_name)(services $?co_serv))
	=>
	(retract ?f1)
		
	(bind ?AddPts 0)
	(loop-for-count (?cnt1 (length$ ?cl_serv)) do
		(if (subsetp (create$ (nth$ ?cnt1 ?cl_serv)) ?co_serv) then
			
			(bind ?AddPts (+ ?AddPts (/ 1 (length$ ?cl_serv))))
			(if (and (= ?AddPts 1) (eq ?sel_by services)) then
				(bind ?AddPts (+ ?AddPts 1))
			)
		)
	)
			
	(assert (Add-Point (company_name $?co_name)(points ?AddPts)))
)

(defrule CheckAmount
	(declare (salience 5))
	(buyer (amount ?cl_amount))
	?f1<-(Check-Amount (company_name $?co_name)(mainSelection ?sel_by))
	?company<-(company(company_name $?co_name)(amount ?co_amount))
	=>
	(retract ?f1)
	
	(if (>= ?co_amount ?cl_amount) then
		(assert (Add-Point (company_name $?co_name)(points 1)))
	)
)

(defrule CheckQuality
	(declare (salience 4))
	(buyer (quality $?cl_quality))
	?f1<-(Check-Quality (company_name $?co_name)(mainSelection ?sel_by))
	?company<-(company(company_name $?co_name)(quality $?co_quality))
	=>
	(retract ?f1)
	
	(loop-for-count (?cnt1 (length$ ?co_quality)) do
		(if (subsetp (create$ (nth$ ?cnt1 ?co_quality)) ?cl_quality) then
			(if(eq ?sel_by quality) then
				(assert (Add-Point (company_name $?co_name)(points 1)))
			else
				(assert (Add-Point (company_name $?co_name)(points 0.5)))
			)
		)
	)
)

(defrule CheckCollisions
	(declare (salience 1))
	?company1<-(company (company_name $?co_name1)(points ?pts1)(position ?pos1))
	?company2<-(company (company_name $?co_name2)(points ?pts2)(position ?pos2&~?pos1))
	(test (= ?pts1 ?pts2))
	=>
	(assert (Collision (company_name1 $?co_name1) (company_name2 $?co_name2)))
)

(defrule Collision-Distribution
	(declare (salience 10))
	
	?f1<-(Collision (company_name1 $?co_name1) (company_name2 $?co_name2))
	(company
		(company_name $?co_name1)
		(position ?co_pos1)
		(points ?co_pts1)
		(amount ?co_amount1)
		(quality $?co_quality1)
		(phone ?co_phone1)
		(price ?co_price1)
		(services $?co_serv1)
		(cable_type ?cable_type1)
	)
	(company
		(company_name $?co_name2)
		(position ?co_pos2)
		(points ?co_pts2)
		(amount ?co_amount2)
		(quality $?co_quality2)
		(phone ?co_phone2)
		(price ?co_price2)
		(services $?co_serv2)
		(cable_type ?cable_type2)
	)
	=>
	(retract ?f1)
	
	(if (and (eq ?cable_type1 optical_cable) (eq ?cable_type2 coaxial_cable))
	then (assert (Add-Point (company_name ?co_name1)(points 0.1)))
	else (if (< (length$ ?co_price1)(length$ ?co_price2))
	then (assert (Add-Point (company_name ?co_name1)(points 0.1)))
	else (if (> (length$ ?co_price1)(length$ ?co_price2))
	then (assert (Add-Point (company_name ?co_name2)(points 0.1)))
	else (if (and (eq ?co_quality1 SD) (eq ?co_quality2 HD))
	then (assert (Add-Point (company_name ?co_name2)(points 0.1)))
	else (if (> (length$ ?co_serv1) (length$ ?co_serv2))
	then (assert (Add-Point (company_name ?co_name1)(points 0.1)))
	else (assert (Add-Point (company_name ?co_name2)(points 0.1)))
	)))))
)

(defrule Position-Distribution
	(declare (salience 5))
	?company1<-(company (company_name $?name1) (points ?pts1) (position ?pos1))
	?company2<-(company (company_name $?name2) (points ?pts2) (position ?pos2&~?pos1))
	(test (and (> ?pts1 ?pts2) (> ?pos1 ?pos2))) 
	=>
	(modify ?company1 (position ?pos2))
	(modify ?company2 (position ?pos1))
)

(defrule Check2
	?f1<-(Check(company_name $?co_name))
	=>
	
)

(defrule WriteCompanies
	(declare (salience -2))
	(company
		(company_name $?co_name)
		(position ?co_pos)
		(points ?co_pts)
		(amount ?co_amount)
		(quality $?co_quality)
		(phone ?co_phone)
		(price ?co_price)
		(services $?co_serv)
		(cable_type ?cable_type)
	)	
	=>
	(if (open "Results.txt" Outfile "a") then 
	(printout Outfile "Company Name: " (implode$ ?co_name)" have position: " ?co_pos)
	(printout Outfile "| Points: " ?co_pts "| Phone: " ?co_phone "| Cable_type: " ?cable_type crlf)
	(printout Outfile "| Price: " ?co_price "| Quality: " (implode$ ?co_quality) crlf)
	(printout Outfile "| Services: " (implode$ ?co_serv) crlf)
	(printout Outfile "Amount: " ?co_amount crlf)
	(printout Outfile "__________________________________________________________________" crlf)
	(close Outfile))
	(printout t "Saved:" ?co_name crlf)
)