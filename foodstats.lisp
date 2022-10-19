;; Copyright 2022 ultasun. All rights reserved.
;; See the LICENSE file. Thank you for reading!
;; -----------------------------------------------------------------------------
;; a system for computing Nutrition Facts totals, given a list of UPC's.

(defpackage :foodstats
  (:use "COMMON-LISP")
  (:import-from "COMMON-LISP-USER" "QUIT"))
(in-package :foodstats)

;; -----------------------------------------------------------------------------
;; the following definitions pertain to re-usable components which are not
;; specific to this package. so they could be copy/pasted somewhere else and
;; would serve a purpose without issue.

;; for menu option 4
;; helper-flatten will flatten a tree by one level. for example:
;; [1]> (in-package :foodstats)
;; #<PACKAGE FOODSTATS>
;; FOODSTATS[2]> (helper-flatten '((((1) (2)) ((3) (4))) (((5) (6)) ((7) (8)))))
;; (((1) (2)) ((3) (4)) ((5) (6)) ((7) (8)))
;; FOODSTATS[3]> (helper-flatten *)
;; ((1) (2) (3) (4) (5) (6) (7) (8))
;; FOODSTATS[4]> (helper-flatten *)
;; (1 2 3 4 5 6 7 8)
;; FOODSTATS[5]> 
(defun helper-flatten (l &optional (result '()))
  "Flatten a tree by one level. Returns a list."
  (if
   l
   (helper-flatten (cdr l) (append result (car l)))
   result))

(defun get-value-or-zero (value)
  "Returns the passed value, or a zero. "
  (if value value 0))

(defun caddddr (list)
  "The cadddr of the cdr of the passed list."
  (cadddr (cdr list)))

;; -----------------------------------------------------------------------------
;; the following definitions pertain to handling dimensional analysis.

;; raw quantity conversion function.
(defun pounds-quantity-to-ounces-quantity (pounds)
  "Returns the quantity, multiplied by sixteen."
  (* pounds 16))

;; raw quantity conversion function.
(defun ounces-quantity-to-grams-quantity (ounces)
  "Returns the quantity, multiplied by 28.3495."
  (* ounces 28.3495))

;; convert pounds to ounces
(defun pounds-dimensional-unit-to-ounces-dimensional-unit (pounds)
  "Convert a pounds dimensional unit, to an ounces dimensional unit.
Returns a cons cell."
  (if
   (eq (cdr pounds) 'pounds)
   (cons (pounds-quantity-to-ounces-quantity (car pounds)) 'ounces)
   nil))

;; convert ounces to grams
(defun ounces-dimensional-unit-to-grams-dimensional-unit (ounces)
  "Convert an ounces dimensional unit, to a grams dimensional unit.
Returns a cons cell."
  (if
   (eq (cdr ounces) 'ounces)
   (cons (ounces-quantity-to-grams-quantity (car ounces)) 'grams)
   nil))

;; convert pounds to grams.
(defun pounds-dimensional-unit-to-grams-dimensional-unit (pounds)
  "Convert a pounds dimensional unit, to a grams dimensional unit.
Returns a cons cell."
  (ounces-dimensional-unit-to-grams-dimensional-unit
   (pounds-dimensional-unit-to-ounces-dimensional-unit pounds)))

;; the "base unit" which calculations need to be performed in, is grams; 
(defun any-dimensional-unit-to-grams-dimensional-unit (dimensional-unit)
  "Convert any dimensional-unit to grams, if possible. Returns a cons cell."
  (let
      ((weight-in-grams 0))
    (cond
      ((eq (cdr dimensional-unit) 'pounds)
       (setf weight-in-grams
	     (pounds-dimensional-unit-to-grams-dimensional-unit
	      dimensional-unit)))
      ((eq (cdr dimensional-unit) 'ounces)
       (setf weight-in-grams
	     (ounces-dimensional-unit-to-grams-dimensional-unit
	      dimensional-unit)))
      ((eq (cdr dimensional-unit) 'grams)
       dimensional-unit)
      ((eq (cdr dimensional-unit) 'calories)
       dimensional-unit)
      (t nil))))

(defun dimensional-unit-p (attribute)
  "Predicate test to detect dimensional unit."
  (and
   attribute ;; nil check
   (listp attribute)
   (or
    (plusp (car attribute)) ;; the scalar value is greater than
    (zerop (car attribute))) ;; or equal to zero
   (symbolp (cdr attribute)))) ;; the unit

;; only works if any-dimensional-unit-to-grams-dimensional-unit works
(defun add-two-dimensional-units (du1 du2)
  "Only works with weights which can be resolved to grams.
Adds two dimensional units, returns a dimensional unit in grams."
  (let
      ((du1-grams
	(any-dimensional-unit-to-grams-dimensional-unit du1))
       (du2-grams
	(any-dimensional-unit-to-grams-dimensional-unit du2)))
    (let
	((result-scalar
	  (+
	   (car du1-grams)
	   (car du2-grams)))
	 (result-unit
	  'grams))
      (cons result-scalar result-unit))))

(defun subtract-two-dimensional-units (du1 du2)
  "Similar to add-two-dimensional-units, but performs a subtraction."
  (let
      ((du1-grams
	(any-dimensional-unit-to-grams-dimensional-unit du1))
       (du2-grams
	(any-dimensional-unit-to-grams-dimensional-unit du2)))
    (let
	((result-scalar
	  (-
	   (car du1-grams)
	   (car du2-grams)))
	 (result-unit
	  'grams))
      (cons result-scalar result-unit))))

;; -----------------------------------------------------------------------------
;; the following definitions pertain to the UPC, history and inventory
;; association lists -- `get`ing particular queries, used by format functions.
;; the UPC database `alist-upc`
;; * is an association list, keyed by a UPC (as a string),
;;   and whose value is a cons cell holding the dimensional unit.
;; the history list `alist-history`
;; * is indexed by universal-seconds, valued by a upc list.
;; the inventory list `alist-inventory`
;; * is indexed by upc (from `alist-upc`)
;;   and valued by an alist keyed by a date string, valued by a quantity.
;; * for example,
(defvar +alist-inv-example+
  '(("541359874631" . (("19-AUG-2023" . (("2.99" . 9)))
		       ("22-JUN-2026" . (("3.99" . 11)))))
    ("665434455782" . (("21-SEP-2027" . (("2.39" . 5)))))))
(defvar +alist-consumption-cost-example+
  '((3860000000 . (("688267027925" ("0.89" . 1))))))

;; returns the association list of expiration date to price alist 
(defun get-inventory-date-group (alist-inventory upc)
  "Returns the alist of expiration date groups for a upc from alist-inventory."
  (cdr (assoc upc alist-inventory :test #'equalp)))

;; returns the association list of price to quantity
(defun get-inventory-price-group (alist-inventory upc date)
  "Returns the alist of prices for an expiration date group in alist-inventory."
  (cdr
   (assoc
    date
    (get-inventory-date-group alist-inventory upc)
    :test #'equalp)))

;; answers the question, "how many of this UPC do I have"? 
(defun get-inventory-total-quantity (alist-inventory upc)
  "Returns a positive integer, or zero if the UPC doesn't exist (or is 0)."
  (apply
   #'+ 
   (mapcar
    (lambda
	(this-expiration-group)
      (apply
       #'+
       (mapcar
	(lambda
	    (this-price-group)
	  (cdr this-price-group))
	(get-inventory-price-group
	 alist-inventory
	 upc
	 (car this-expiration-group)))))
    (get-inventory-date-group alist-inventory upc))))

;; answers the question, "how many of the UPC which expire on this day?"
(defun get-inventory-date-group-quantity (alist-inventory upc date)
  "Returns a positive integer, or zero if the UPC/date doesn't exist (or is 0)."
  (apply
   #'+
   (mapcar
    (lambda
	(this-price-group)
      (cdr this-price-group))
    (get-inventory-price-group
     alist-inventory
     upc
     date))))

;; answers the question, "how many of the UPC which
;; cost this much and expire on this day?"
(defun get-inventory-price-group-quantity (alist-inventory upc date price)
  "Returns a positive integer, or zero if it doesn't exist, or is 0."
  (cdr
   (assoc
    price
    (get-inventory-price-group alist-inventory upc date)
    :test #'equalp)))

;; answers the question, "what is the last date group with a quantity > 0?"
(defun get-inventory-last-date-group
    (alist-inventory
     upc
     &optional
       (date-groups
	(get-inventory-date-group
	 alist-inventory upc)))
  ""
  (if
   date-groups
   (if
    (>
     (get-inventory-date-group-quantity
      alist-inventory
      upc
      (caar (last date-groups)))
     0)
    (caar (last date-groups))
    ;; else, recursive call
    (get-inventory-last-date-group
     alist-inventory
     upc
     (butlast date-groups)))
   nil))

;; answers the question, "what is the last price group with a quantity > 0?"
(defun get-inventory-last-price-group
    (alist-inventory
     upc
     date
     &optional
       (price-groups
	(get-inventory-price-group
	 alist-inventory upc date)))
  ""
  (if
   price-groups ;; needs to end the recursive search
   (if
    (>
     (get-inventory-price-group-quantity
      alist-inventory
      upc
      date
      (caar (last price-groups)))
     0)
    (caar (last price-groups))
    ;; else, recursive call
    (get-inventory-last-price-group
     alist-inventory
     upc
     date
     (butlast price-groups)))
   nil))

;; get an attribute from a UPC.
(defun get-upc-attribute (alist-upc upc attribute)
  "Return some attribute from a UPC."
  (cdr (assoc attribute (cdr (assoc upc alist-upc :test #'equalp)))))

;; return all attributes for a UPC.
(defun get-upc-attributes (alist-upc upc)
  (cdr (assoc upc alist-upc :test #'equalp)))

;; get an attribute from a UPC,
;; but only if that attribute is a dimensional-unit.
(defun get-dimensional-unit-from-upc-attribute (alist-upc upc attribute)
  "Return some attribute's dimensional unit from a UPC.  Example: (19 . grams).
Returns nil if the attribute doesn't exist, or isn't a dimensional-unit."
  (let
      ((result (get-upc-attribute alist-upc upc attribute)))
    (if
     (dimensional-unit-p result)
     result
     nil)))

;; for menu option 4
;; there is likely to be a standard pattern somewhere on how to do this.
;; TODO find the correct pattern to reduce the footprint of this function
(defun get-upc-list-between-times (alist-history time-range)
  "Given the universal-seconds range in the second argument, 
search the first argument for all the UPC's which had been used
(within that interval.)"
  (helper-flatten 
   (mapcar
    (lambda
	(this-time)
      (if
       (and (>= (car this-time) (car time-range))
	    (<= (car this-time) (cdr time-range)))
       (cdr this-time)))
    alist-history)))

(defun get-role-alias-upc-list (alist-role-alias role-alias)
  "Given the association list mapping role-aliases to a list of UPC's which
fulfill that alias, and a role-alias to look up, return the list of UPC's which
are fulfilled by that role-alias."
  (cdr (assoc role-alias alist-role-alias :test #'equalp)))

(defun get-role-alias-last-upc-ignore-inventory
    (alist-role-alias
     role-alias)
  "Returns the last UPC assigned to a role-alias list, using the function
GET-ROLE-ALIAS-UPC-LIST to retrieve the entire role-alias UPC list."
  (car (last (get-role-alias-upc-list alist-role-alias role-alias))))

(defun get-role-alias-first-upc-ignore-inventory
    (alist-role-alias
     role-alias)
  "Returns the first UPC in the role-alias UPC list, using the function
GET-ROLE-ALIAS-UPC-LIST to retrieve the entire role-alias UPC list."
  (first (get-role-alias-upc-list alist-role-alias role-alias)))

(defun get-role-alias-upc-has-inventory
    (alist-upc
     alist-inventory
     alist-role-alias
     role-alias
     &optional
       (minimum-quantity 1)
       (search-list
	(get-role-alias-upc-list
	 alist-role-alias
	 role-alias)))
  "Returns nil if no UPC for this role alias has inventory.
Otherwise returns a string "
  (if
   search-list
   (if
    (>= 
     (get-inventory-total-quantity
      alist-inventory
      (car
       search-list))
     minimum-quantity)
    (car search-list) ;; return this UPC
    (get-role-alias-upc-has-inventory
     alist-upc
     alist-inventory
     alist-role-alias
     role-alias
     minimum-quantity
     (cdr search-list)))
   nil))

(defun get-role-alias-upc-has-inventory-or-leftover
    (alist-upc
     alist-inventory
     alist-role-alias
     alist-upc-leftover
     role-alias
     &optional ;; possibly make minimum-quantity automatic from 
       (minimum-quantity 1) ;; the information in minimum-dimensional-unit
       (minimum-dimensional-unit '(1 . grams)) ;; and alist-upc
       (skip-list '())
       (search-list
	(get-role-alias-upc-list
	 alist-role-alias
	 role-alias)))
  ""
  (if
   search-list
   (if ;; first check the leftovers 
    (and
     (not
      (member
       (car search-list)
       skip-list
       :test #'equalp))
     (assoc
      (car search-list)
      alist-upc-leftover
      :test #'equalp)
     (>=
      (cadar
       (calculate-recipe-dimensional-units-from-portion-percentages
	alist-upc
	(list
	 (assoc
	  (car search-list)
	  alist-upc-leftover
	  :test #'equalp))))
      (car
       (any-dimensional-unit-to-grams-dimensional-unit
	minimum-dimensional-unit))))
    (values
     (car search-list) ;; return this leftover if it is suitable
     (cdr
      (assoc
       (car search-list)
       alist-upc-leftover
       :test #'equalp)))
    (if ;; "else if" check inventory for a new unit
     (and
      (not
       (member
	(car search-list)
	skip-list
	:test #'equalp))
      (>=
       (get-inventory-total-quantity
	alist-inventory
	(car
	 search-list))
       minimum-quantity))
     (car search-list) ;; return this UPC, else 
     (get-role-alias-upc-has-inventory-or-leftover ;; continue the search
      alist-upc
      alist-inventory
      alist-role-alias
      alist-upc-leftover
      role-alias
      minimum-quantity
      minimum-dimensional-unit
      skip-list
      (cdr search-list))))
   nil))

(defun get-random-upc (&optional (alist-upc '()) (length 12))
  "Returns a random number of 'length', which is 12 by default.
Calls recursively until a non-matching UPC from alist-upc is found.
Returns a string."
  (let
      ((candidate
	(write-to-string
	 (+ (expt 10 (- length 1))
	    (random (expt 10 (- length 1)))))))
    (if
     (assoc candidate alist-upc :test #'equalp)
     (get-random-upc alist-upc length)
     candidate)))

;; returns the first UPC for the alias
;; alist-role-alias contains a LIFO queue for each role
;; this way, the next UPC is the freshest one, and not the oldest one
;;(defun get-next-upc-from-role-alias (alist-role-alias role-alias)
;;  "LIFO queue for alist-role-history "
;;  (car (get-role-alias-upc-list alist-role-alias role-alias)))

(defun get-recipe-upc-dimensional-unit-list
    (alist-recipe
     recipe-name)
  "Returns the upc list as dimensional-units.
Returns an association list."
  (cdr (assoc recipe-name alist-recipe :test #'equalp)))

(defun get-recipe-upc-percent-list-ignore-inventory
    (alist-upc
     alist-recipe
     alist-role-alias
     recipe-name)
  ""
  (calculate-recipe-portion-percentages-from-dimensional-units
   alist-upc
   (convert-recipe-role-alias-keys-to-upc-keys-ignore-inventory
    alist-upc
    alist-recipe
    alist-role-alias
    (get-recipe-upc-dimensional-unit-list
     alist-recipe
     recipe-name))))

(defun get-upc-leftover-percent-or-zero
    (alist-upc-leftover
     upc)
  "Return the leftover UPC or a zero."
  (let
      ((upc-leftover
	(cdr
	 (assoc
	  upc
	  alist-upc-leftover
	  :test #'equalp))))
    (if
     (numberp upc-leftover)
     upc-leftover
     0.0)))


(defun get-accumulated-cost-quantity-pairs
    (alist-inventory
     tier-one-key
     tier-two-key)
  ""
  (cdr
   (assoc
    tier-two-key
    (cdr
     (assoc
      tier-one-key
      alist-inventory
      :test #'equalp))
    :test #'equalp)))

(defun get-accumulated-cost-quantity-pair
    (alist-inventory
     tier-one-key
     tier-two-key
     tier-three-key)
  ""
  (assoc
   tier-three-key
   (cdr
    (assoc
     tier-two-key
     (cdr
      (assoc
       tier-one-key
       alist-inventory
       :test #'equalp))
     :test #'equalp))
   :test #'equalp))

;; get the original UPC list to cook something which is in alist-recipe-leftover
(defun get-upc-list-from-alist-recipe-leftover
    (alist-recipe-leftover
     recipe-name
     recipe-cook-stamp)
  ""
  (assoc
   recipe-cook-stamp
   (cdr (assoc recipe-name alist-recipe-leftover :test #'equalp))))

;; "how to use mapcar to search" (as opposed to list-eater search)
(defun get-role-alias-from-leftover
    (alist-upc
     alist-role-alias
     alist-upc-leftover
     role-alias
     &optional
       (minimum-dimensional-unit '(1 . grams)))
  ""
  (mapcan
   (lambda
       (this-leftover)
     (if
      (member
       (car this-leftover)
       (get-role-alias-upc-list
	alist-role-alias
	role-alias)
       :test #'equalp)
      (if
       (>=
	(cadar 
	 (calculate-recipe-dimensional-units-from-portion-percentages
	  alist-upc
	  (list
	   (assoc
	    (car this-leftover)
	    alist-upc-leftover
	    :test #'equalp))))
	(car
	 (any-dimensional-unit-to-grams-dimensional-unit
	  minimum-dimensional-unit)))
       (list this-leftover))))
   alist-upc-leftover))

;; compliments get-role-alias-from-leftover, uses mapcar to search
(defun get-role-alias-from-inventory
    (alist-upc
     alist-inventory
     alist-role-alias
     role-alias
     &optional
       (minimum-dimensional-unit '(1 . grams)))
  ""
  (mapcan
   (lambda
       (this-inventory-record)
     (if
      (member
       (car this-inventory-record)
       (get-role-alias-upc-list
	alist-role-alias
	role-alias)
       :test #'equalp)
      (let
	  ((inventory-total-quantity
	    (* ;; multiply by 1.0 to convert to "percentage portion" which
	     1.0  ;; is in the range of [0, 1)+ 
	     (get-inventory-total-quantity
	      alist-inventory
	      (car this-inventory-record)))))
	(if
	 (>=
	  (cadar
	   (calculate-recipe-dimensional-units-from-portion-percentages
	    alist-upc
	    (list
	     (cons
	      (car this-inventory-record)
	      inventory-total-quantity))))
	  (car
	   (any-dimensional-unit-to-grams-dimensional-unit
	    minimum-dimensional-unit)))
	 (list
	  (cons
	   (car this-inventory-record)
	   inventory-total-quantity))))))
   alist-inventory))

;; return the percentage of a recipe remaining
(defun get-recipe-leftover-remaining-percentage
    (alist-recipe-leftover
     recipe-name
     cook-stamp)
  ""
  (cadr
   (assoc
    cook-stamp
    (cdr
     (assoc
      recipe-name
      alist-recipe-leftover
      :test #'equalp)))))

;; this function is used to incrementally build an entire alist-upc database.
;; building alist-upc database 'by hand' is shown in `alist-upc.example.lisp`
;; and this file may be found in the `data` directory.
;;
;; note this is also used for building recipes, so it may need to be
;; renamed in the future to #'adjust-alist-attribute.
(defun adjust-alist-upc-attribute (alist-upc upc attribute)
  "Create or Update an attribute for the given UPC.
The UPC will be created or updated as necessary.
Returns alist-upc, with the added or updated association list structure."
  (acons
   upc
   (let
       ((upc-attributes
	 (cdr
	  (assoc upc alist-upc :test #'equalp))))
     (append
      (remove
       (car attribute)
       upc-attributes
       :key #'car
       :test #'equalp)
      (list attribute)))
   (remove upc alist-upc :key #'car :test #'equalp)))

;; this function is used to incrementally build the inventory database.
;; see +alist-inv-example+ for an illustration of the embedded alist structure.
;; alist-inventory may be built "by hand" as a let statement in the text file.
(defun adjust-alist-inventory (alist-inventory upc date price quantity)
  "Add or update the UPC's quantity in an expiration group.
Returns a new alist-inventory."
  (acons
   upc
   (acons
    date
    (acons
     price
     quantity
     (remove
      price
      (get-inventory-price-group alist-inventory upc date)
      :test #'equalp :key #'car))
    (remove
     date
     (get-inventory-date-group alist-inventory upc)
     :test #'equalp :key #'car))
   (remove upc alist-inventory :key #'car :test #'equalp)))

;; update the list of UPCs which fulfill a certain role
(defun adjust-alist-role-alias (alist-role-alias upc role-alias)
  "Add or update to the list of UPCs which fulfill a ceratin role.
Returns a new alist-alias-role."
  (acons
   role-alias
   (append
    (list upc)
    (get-role-alias-upc-list alist-role-alias role-alias))
   (remove
    role-alias
    alist-role-alias
    :key #'car
    :test #'equalp)))

(defun adjust-alist-percentage-list (alist-percentage-list key value)
  (acons
   key
   value
   (remove
    key
    alist-percentage-list
    :key #'car :test #'equalp)))

;; used to update the percentage of a leftover recipe "cook" remaining
(defun adjust-alist-recipe-leftover-percentage
    (alist-recipe-leftover
     recipe-name
     recipe-cook-stamp
     new-percentage)
  ""
  (acons
   recipe-name
   (acons
    recipe-cook-stamp
    (cons
     new-percentage
     (cddr
      (assoc
       recipe-cook-stamp
       (cdr
	(assoc
	 recipe-name
	 alist-recipe-leftover
	 :test #'equalp)))))
    (remove
     recipe-cook-stamp
     (cdr
      (assoc
       recipe-name
       alist-recipe-leftover
       :test #'equalp))
     :key #'car :test #'equalp))
   (remove
    recipe-name
    alist-recipe-leftover
    :key #'car :test #'equalp)))

;; take a recipe with role-alias entries and convert them to
;; upc entries but only if there is inventory for those UPC's
;; does not care if the list is of percentage-portions or dimensional-units
(defun convert-recipe-role-alias-keys-to-upc-keys
    (alist-upc
     alist-recipe
     alist-inventory
     alist-role-alias
     recipe-using-role-aliases
     &optional
       (new-upc-list '()))
  ""
  (if
   recipe-using-role-aliases
   (let
       ((next-upc
	 (get-role-alias-upc-has-inventory
	  alist-upc
	  alist-inventory
	  alist-role-alias
	  (caar recipe-using-role-aliases))))
     (convert-recipe-role-alias-keys-to-upc-keys
      alist-upc
      alist-recipe
      alist-inventory
      alist-role-alias
      (cdr recipe-using-role-aliases)
      (if
       next-upc
       (acons
	next-upc
	(cdar recipe-using-role-aliases)
	new-upc-list)
       (acons
	(caar recipe-using-role-aliases)
	(cdar recipe-using-role-aliases)
	new-upc-list))))
   new-upc-list))

;; need to add minimum-dimensional-unit
(defun convert-recipe-role-alias-keys-to-upc-keys-check-leftover
    (alist-upc
     alist-recipe
     alist-inventory
     alist-role-alias
     alist-upc-leftover
     recipe-using-role-aliases
     &optional
       (minimum-dimensional-unit '(1 . grams))
       (accumulated-dimensional-unit '(0 . grams))
       (new-upc-list '())
       (skip-list '()))
  ""
  (if
   recipe-using-role-aliases
   (let
       ((next-upc ;; verify size
	 (get-role-alias-upc-has-inventory-or-leftover
	  alist-upc
	  alist-inventory
	  alist-role-alias
	  alist-upc-leftover ;; update to add quick math to determine the 
	  (caar recipe-using-role-aliases) ;; unit quantity required
	  1
	  '(0 . grams)
	  skip-list)))
     (convert-recipe-role-alias-keys-to-upc-keys-check-leftover
      alist-upc
      alist-recipe
      alist-inventory
      alist-role-alias
      alist-upc-leftover
      minimum-dimensional-unit
      accumulated-dimensional-unit
      (cdr recipe-using-role-aliases)
      (if
       next-upc
       (acons
	next-upc
	(cdar recipe-using-role-aliases)
	new-upc-list)
       (acons
	(caar recipe-using-role-aliases)
	(cdar recipe-using-role-aliases)
	new-upc-list))
      (append skiplist (list next-upc))))
   new-upc-list))

;; list eater function eats recipe-using-role-aliases
;; and builds new-upc-list with the correct quantities available
;; in alist-upc-leftover being used before accessing unopened alist-inventory
;; menu option 43
(defun convert-recipe-role-alias-keys-to-upc-keys-sum-leftover
    (alist-upc
     alist-recipe
     alist-inventory
     alist-role-alias
     alist-upc-leftover
     recipe-using-role-aliases
     &optional
       (new-upc-list '()))
  ""
  (if
   recipe-using-role-aliases
   (let
       ((this-role-total-required-dimensional-unit
	 (any-dimensional-unit-to-grams-dimensional-unit
	  (cdar
	   recipe-using-role-aliases))))
     (let
	 ((list-upc-in-leftover-sum-to-role-total-required
	   (calculate-upc-list-in-leftover-sum-to-total-required
	    alist-upc
	    alist-role-alias
	    alist-upc-leftover
	    role-alias
	    this-role-total-required-dimensional-unit)))
       (values
	list-upc-in-leftover-sum-to-role-total-required
	this-role-total-required-dimensional-unit)))))


;; same thing as #'convert-recipe-role-alias-keys-to-upc-keys but ignores
;; inventory and returns the last upc assigned to the role-alias
(defun convert-recipe-role-alias-keys-to-upc-keys-ignore-inventory
    (alist-upc
     alist-recipe
     alist-role-alias
     recipe-using-role-aliases
     &optional
       (new-upc-list '()))
  ""
  (if
   recipe-using-role-aliases
   (let
       ((next-upc
	 (get-role-alias-first-upc-ignore-inventory
	  alist-role-alias
	  (caar recipe-using-role-aliases))))
     (convert-recipe-role-alias-keys-to-upc-keys-ignore-inventory
      alist-upc
      alist-recipe
      alist-role-alias
      (cdr recipe-using-role-aliases)
      (if
       next-upc
       (acons
	next-upc
	(cdar recipe-using-role-aliases)
	new-upc-list)
       (acons
	(caar recipe-using-role-aliases)
	(cdar recipe-using-role-aliases)
	new-upc-list))))
   new-upc-list))

(defun convert-recipe-role-alias-keys-to-upc-keys-use-up-leftover
    (alist-upc
     alist-recipe
     alist-inventory
     alist-role-alias
     alist-upc-leftover
     recipe-using-role-aliases
     &optional
       (new-upc-list '()))
  ""
  (if
   recipe-using-role-aliases
   (let
       ((total-leftover-options
	 (get-role-alias-from-leftover
	  alist-upc
	  alist-role-alias
	  alist-upc-leftover
	  (caar recipe-using-role-aliases))))
     (let ;; result-calculation is a list containing 3 elements:
	 ((result-calculation ;; result-upc-list, alist-upc-leftover
	   (calculate-role-alias-subtraction-from-leftover ;; and 
	    alist-upc           ;; result-recipe-using-role-aliases
	    alist-role-alias    
	    alist-upc-leftover
	    (caar recipe-using-role-aliases)
	    (any-dimensional-unit-to-grams-dimensional-unit
	     (cdar recipe-using-role-aliases)))))
       (convert-recipe-role-alias-keys-to-upc-keys-use-up-leftover
	alist-upc
	alist-recipe
	alist-inventory
	alist-role-alias
	(cadr result-calculation)
	(cdr recipe-using-role-aliases)
	(append
	 new-upc-list
	 (car result-calculation)
	 (caddr result-calculation)))))
   new-upc-list))

;; returns result-upc-list, alist-upc-leftover, and
;; the recipe-using-role-aliases which still need to be resolved
;; from inventory (they were unavailable as open leftovers)
(defun calculate-role-alias-subtraction-from-leftover
    (alist-upc
     alist-role-alias
     alist-upc-leftover
     role-alias
     &optional
       (requirement-dimensional-unit '(0 . grams))
       (these-applicable-leftovers
	(calculate-recipe-dimensional-units-from-portion-percentages
	 alist-upc
	 (get-role-alias-from-leftover
	  alist-upc
	  alist-role-alias
	  alist-upc-leftover
	  role-alias)))
       (result-upc-list '())
       (result-recipe-using-role-alias '()))
  ""
  (if
   these-applicable-leftovers
   (let
       ((grams-available-this-upc
	 (cadar these-applicable-leftovers))
	(grams-needed
	 (car requirement-dimensional-unit)))
     (cond
       ((<= grams-needed grams-available-this-upc) 
	(calculate-role-alias-subtraction-from-leftover
	 alist-upc
	 alist-role-alias
	 (adjust-alist-percentage-list ;; result-alist-upc-leftover
	  alist-upc-leftover
	  (caar these-applicable-leftovers)
	  (- 
	   (cdar
	    (calculate-recipe-portion-percentages-from-dimensional-units
	     alist-upc
	     (list
	      (car
	       these-applicable-leftovers))))
	   (cdar
	    (calculate-recipe-portion-percentages-from-dimensional-units
	     alist-upc
	     (list
	      (cons
	       (caar these-applicable-leftovers)
	       requirement-dimensional-unit))))))
	 role-alias
	 (subtract-two-dimensional-units
	  requirement-dimensional-unit
	  (cons
	   grams-available-this-upc
	   'grams))
	 (cdr these-applicable-leftovers)
	 (append ;; result-upc-list 
	  result-upc-list
	  (list
	   (cons
	    (caar these-applicable-leftovers)
	    (cons grams-needed 'grams))))
	 (append ;; result-recipe-using-role-aliases
	  result-recipe-using-role-alias
	  (list
	   (cons
	    role-alias
	    (cons
	     0 ;; 
	     'grams))))))
       ((> grams-needed grams-available-this-upc)
	(calculate-role-alias-subtraction-from-leftover
	 alist-upc
	 alist-role-alias
	 (adjust-alist-percentage-list ;; result-alist-upc-leftover
	  alist-upc-leftover
	  (caar these-applicable-leftovers)
	  0)
	 role-alias
	 (subtract-two-dimensional-units
	  requirement-dimensional-unit
	  (cons
	   grams-available-this-upc
	   'grams))
	 (cdr these-applicable-leftovers)
	 (append ;; result-upc-list
	  result-upc-list
	  (list (car these-applicable-leftovers)))
	 (append ;; result-recipe-using-role-aliases
	  result-recipe-using-role-alias
	  (list
	   (cons
	    role-alias
	    (cons
	     (- grams-needed grams-available-this-upc)
	     'grams))))))
       (t
	"fatal: calculate-role-alias-subtraction-from-leftover cond fault")))
   (list
    result-upc-list
    alist-upc-leftover
    (if
     (and
      (> (car requirement-dimensional-unit) 0)
      (not result-recipe-using-role-alias)) ;; sticky
     (append
      result-recipe-using-role-alias
      (list
       (cons
	role-alias
	requirement-dimensional-unit)))
     result-recipe-using-role-alias))))

;; not recursive, will only process the first applicable leftover found
(defun calculate-role-alias-subtraction-from-leftover%
    (alist-upc
     alist-role-alias
     alist-upc-leftover
     role-alias
     &optional
       (requirement-dimensional-unit '(0 . grams))
       (these-applicable-leftovers
	(calculate-recipe-dimensional-units-from-portion-percentages
	 alist-upc
	 (get-role-alias-from-leftover
          alist-upc
          alist-role-alias
          alist-upc-leftover
          role-alias)))
       (result-upc-list '())
       (result-recipe-using-role-alias '()))
  ""
  (if
   these-applicable-leftovers
   (let
       ((grams-available-this-upc
         (cadar these-applicable-leftovers))
        (grams-needed
         (car requirement-dimensional-unit)))
     (cond
       ((<= grams-needed grams-available-this-upc) 
        (list 
         (append ;; result-upc-list 
          result-upc-list
          (list
           (cons
            (caar these-applicable-leftovers)
            (cons grams-needed 'grams))))
         (adjust-alist-percentage-list ;; result-alist-upc-leftover
          alist-upc-leftover
          (caar these-applicable-leftovers)
          (- 
           (cdar
            (calculate-recipe-portion-percentages-from-dimensional-units
             alist-upc
             (list
	      (car
	       these-applicable-leftovers))))
           (cdar
            (calculate-recipe-portion-percentages-from-dimensional-units
             alist-upc
             (list
	      (cons
	       (caar these-applicable-leftovers)
	       requirement-dimensional-unit))))))
         (append ;; result-recipe-using-role-aliases
          result-recipe-using-role-alias
          (list
           (cons
            role-alias
            (cons
             0 ;; 
             'grams))))))
       ((> grams-needed grams-available-this-upc)
        (list
         (append ;; result-upc-list
          result-upc-list
          (list (car these-applicable-leftovers)))
         (adjust-alist-percentage-list ;; result-alist-upc-leftover
          alist-upc-leftover
          (caar these-applicable-leftovers)
          0)
         (append ;; result-recipe-using-role-aliases
          result-recipe-using-role-alias
          (list
           (cons
            role-alias
            (cons
             (- grams-needed grams-available-this-upc)
             'grams))))))
       (t
        "default choice")))))

;; figure out total inventory value for alist-inventory and consumption-cost
(defun calculate-inventory-total-value (alist-inventory)
  "total value in alist-inventory, alist-consumption-cost, or similar."
  (apply
   #'+ 
   (helper-flatten
    (helper-flatten
     (mapcar
      (lambda
	  (this-tier1-record)
	(mapcar
	 (lambda
	     (this-tier2-record)
	   (mapcar
	    (lambda
		(this-tier3-record)
	      (*
	       (read-from-string
		(car this-tier3-record))
	       (cdr this-tier3-record)))
	    (cdr this-tier2-record)))
	 (cdr this-tier1-record)))
      alist-inventory)))))

;; *** RETURNS ALIST-INVENTORY OR INTEGER ***
;; consume a quantity of upc from alist-inventory, while forbidding overdrafts
;; and round-robin consuming date/price quantities (easy to implement)
;; if (> quantity 1) then the function is called recursively for each decrement.
(defun consume-from-alist-inventory
    (alist-upc alist-inventory upc quantity)
  "Consume a quantity of upc from alist-inventory.
Follows a round-robin approach to consuming expiration-date/price groups.
Forbids overdrafting an inventory balance.
Returns the updated alist-inventory reflecting the new available quantities, or,
will return negative integer of overdraft amount.
*** RETURNS ALIST-INVENTORY OR INTEGER ***"
  (let
      ((total-quantity-in-inventory
	(get-inventory-total-quantity
	 alist-inventory upc)))
    (if ;; sanity check, then the "good case" where no overdaft occurs
     (<= quantity total-quantity-in-inventory) 
     (if
      (= quantity 0)
      alist-inventory ;; base case
      (let ;; recursive case
	  ((last-date-group
	    (get-inventory-last-date-group
	     alist-inventory upc)))
	(let
	    ((last-price-group
	      (get-inventory-last-price-group
	       alist-inventory upc last-date-group))
	     (total-quantity-in-inventory
	      (get-inventory-total-quantity
	       alist-inventory upc)))
	  (consume-from-alist-inventory
	   alist-upc
	   (if
	    (> total-quantity-in-inventory 0)
	    (adjust-alist-inventory ;; returns the new alist-inventory
	     alist-inventory
	     upc
	     last-date-group
	     last-price-group
	     (-
	      (get-inventory-price-group-quantity
	       alist-inventory
	       upc
	       last-date-group
	       last-price-group)
	      1))
	    alist-inventory)
	   upc
	   (if
	    (> total-quantity-in-inventory 0)
	    (- quantity 1)
	    0)))))
     ;; below is the "bad case" where the caller attempts to overdraft,
     (- total-quantity-in-inventory quantity)))) ;; returns negative integer

;; same as consume-from-alist-inventory but takes 'alist-accounting' which is
;;  an alist keyed similar (by seconds) to alist-history , but is valued by
;;  an association list indexing UPC to a cons of (string) cost and quantity,
;; in the similar way within their date groups in alist-inventory.

;; returns alist-inventory and alist-consumption-cost
(defun consume-from-alist-inventory-adjust-consumption-cost
    (alist-upc alist-inventory alist-consumption-cost upc quantity
     &optional (consumption-time (get-universal-time)))
  "see documentation for consume-from-alist-inventory.
but now this function returns a list containing the resulting
alist-inventory and alist-consumption-cost."
  (let
      ((total-quantity-in-inventory
	(get-inventory-total-quantity
	 alist-inventory upc)))
    (if
     (<= quantity total-quantity-in-inventory)
     (if
      (= quantity 0)
      (values alist-inventory alist-consumption-cost);; base case
      (let ;; recursive case
	  ((last-date-group
	    (get-inventory-last-date-group
	     alist-inventory upc)))
	(let
	    ((last-price-group
	      (get-inventory-last-price-group
	       alist-inventory upc last-date-group))
	     (total-quantity-in-inventory
	      (get-inventory-total-quantity
	       alist-inventory upc)))
	  (let
	      ((result-alist-inventory
		(if
		 (> total-quantity-in-inventory 0)
		 (adjust-alist-inventory
		  alist-inventory
		  upc
		  last-date-group
		  last-price-group
		  (-
		   (get-inventory-price-group-quantity
		    alist-inventory
		    upc
		    last-date-group
		    last-price-group)
		   1))))
	       (result-alist-consumption-cost
		(adjust-alist-inventory ;; bear with me
		 alist-consumption-cost 
		 consumption-time 
		 upc
		 last-price-group
		 (+
		  1
		  (get-value-or-zero
		   (cdr 
		    (get-accumulated-cost-quantity-pair
		     alist-consumption-cost
		     consumption-time
		     upc
		     last-price-group)))))))
	    (consume-from-alist-inventory-adjust-consumption-cost
	     alist-upc
	     result-alist-inventory
	     result-alist-consumption-cost
	     upc
	     (if
	      (> total-quantity-in-inventory 0)
	      (- quantity 1)
	      0)
	     consumption-time)))))
     ;; "bad case"
     (- total-quantity-in-inventory quantity))))

;; -----------------------------------------------------------------------------
;; the following definitions pertain to calculating Nutrition Facts data.
;; they all require their arguments to be proper dimensional-units
;; (which are cons cells)
;; most require alist-upc, which is generated with #'adjust-alist-upc-attribute

;; used to calculate exactly how much is in a container, for variable size
;;   (for example, buying a 1-pound pack of ground beef,
;;                 is usually weighing more than 1 pound.)
;; this can also be used if there is label information missing.
;; or suppose if the listed serving size is in teaspoons which is a volume.
(defun calculate-servings-per-container-from-container-weight
    (serving-size container-weight)
  "Determine the number of servings in a container,
given a serving-size and container-weight.
Returns a cons cell."
  (cons
   (/ 
    (car (any-dimensional-unit-to-grams-dimensional-unit container-weight))
    (car (any-dimensional-unit-to-grams-dimensional-unit serving-size)))
   'servings))

(defun calculate-percentage-of-dimensional-units
    (upc-list-dimensional-units multiplier)
  "Returns the given list with the dimensional units multiplied by multiplier.
Returns an association list."
  (mapcar
   (lambda (this-upc)
     (cons
      (car this-upc)
      (cons
       (*
	(cadr this-upc)
	multiplier)
       (cddr this-upc))))
   upc-list-dimensional-units))

(defun calculate-subtract-portion-percentage
    (upc-list-portion-percentages percentage-to-subtract)
  (mapcar
   (lambda (this-upc)
     (cons
      (car this-upc)
      (cons
       (-
	(cadr this-upc)
	percentage-to-subtract)
       (cddr this-upc))))
   upc-list-portion-percentages))

;; if I ate so many grams/pounds/ounces of X,
;; then how much of attribute (protein, for example) did I eat?
;; NOTE this function is not currently in use anywhere.
(defun calculate-attribute-by-weight
    (alist-upc upc weight-of-upc-consumed attribute
     &optional (default-unit 'grams))
  "Given an alist-upc, a UPC and a dimensionally-valid weight-of-upc-consumed;
return how much attribute was consumed. Returns a cons cell."
  (let
      ((attribute-per-serving-grams
	(any-dimensional-unit-to-grams-dimensional-unit
	 (get-dimensional-unit-from-upc-attribute
	  alist-upc upc attribute)))
       (serving-size-grams
	(any-dimensional-unit-to-grams-dimensional-unit 
	 (get-dimensional-unit-from-upc-attribute
	  alist-upc upc 'serving-size)))
       (servings-per-container
	(get-dimensional-unit-from-upc-attribute
	 alist-upc upc 'servings-per-container)))
    (if
     (dimensional-unit-p attribute-per-serving-grams)
     (cons
      (*
       (car
	(any-dimensional-unit-to-grams-dimensional-unit
	 weight-of-upc-consumed))
       (/
	(car attribute-per-serving-grams)
	(car serving-size-grams)))
      (cdr
       (get-dimensional-unit-from-upc-attribute
	alist-upc upc attribute)))
     (cons 0 default-unit)))) ;; default result is 0 default-units.

;; if I ate a percentage of a container of X,
;; then how much attribute (protein, for example) did I get?
(defun calculate-attribute-by-percent
    (alist-upc upc percentage-portion attribute
     &optional (default-unit 'grams))
  "Given an alist-upc, a UPC and percentage-portion [0.0, 1.0];
return how much attribute was consumed.
Returns a cons cell."
  (let
      ((attribute-per-serving-grams
	(any-dimensional-unit-to-grams-dimensional-unit
	 (get-dimensional-unit-from-upc-attribute
	  alist-upc upc attribute)))
       (serving-size-grams
	(any-dimensional-unit-to-grams-dimensional-unit
	 (get-dimensional-unit-from-upc-attribute
	  alist-upc upc 'serving-size)))
       (servings-per-container
	(get-dimensional-unit-from-upc-attribute
	 alist-upc upc 'servings-per-container)))
    (if
     (dimensional-unit-p attribute-per-serving-grams)
     (cons
      (*
       percentage-portion
       (*
	(car
	 (get-dimensional-unit-from-upc-attribute
	  alist-upc upc attribute))
	(car servings-per-container)))
      (cdr
       (get-dimensional-unit-from-upc-attribute
	alist-upc upc attribute)))
     (cons 0 default-unit)))) ;; default result is 0 default-units.

;; a "recipe" is an association list,
;; whose "key / value" pair is "upc / portion-percentage"
;; so list-recipes will be replacing list-recipe-upcs
(defun calculate-nutrition-report-from-recipe-by-portion-percentages
    (alist-upc
     list-recipe
     list-foodstats
     portion-percentage)
  ""
  (let
      ((alist-result '()))
    (mapcan
     (lambda
	 (this-recipe-upc)
       (mapcar
	(lambda
	    (this-nutrition-fact)
	  (if
	   (assoc this-nutrition-fact alist-result)
	   (setf
	    alist-result ;; if this attribute has accumulation,
	    (append ;; then accumulate onto the already stored value
	     (remove
	      this-nutrition-fact alist-result :key #'car)
	     (list
	      (cons
	       this-nutrition-fact
	       (cons
		(+
		 (car
		  (calculate-attribute-by-percent
		   alist-upc
		   (car this-recipe-upc) ;; car position contains upc
		   (*
		    portion-percentage
		    (cdr this-recipe-upc)) ;; distributive property
		   this-nutrition-fact))
		 (cadr (assoc this-nutrition-fact alist-result)))
		(cddr (assoc this-nutrition-fact alist-result)))))))
	   (if
	    (dimensional-unit-p
	     (get-upc-attribute
	      alist-upc
	      (car this-recipe-upc) this-nutrition-fact))
	    (setf alist-result ;; else, this attribute is a first
		  (append
		   alist-result
		   (list
		    (cons
		     this-nutrition-fact
		     (calculate-attribute-by-percent
		      alist-upc
		      (car this-recipe-upc) ;; car position is upc string
		      (*
		       portion-percentage
		       (cdr this-recipe-upc)) ;; distributive property
		      this-nutrition-fact)))))
	    nil)))
	list-foodstats))
     list-recipe)
    alist-result))

;; converts an alist of upcs to dimensional-units,
;; into an alist of upc's and portion-percentages
;; this is possible because each UPC knows it's weight,
;; and the dimensional-unit in list-recipe-by-dimensional-units is some
;; portion to what's stored in the upc.
;; so this function returns an association list of upc's to ratios.
(defun calculate-recipe-portion-percentages-from-dimensional-units
    (alist-upc
     list-recipe-by-dimensional-units)
  ""
  (mapcar
   (lambda
       (this-recipe-element)
     (let
	 ((total-container-weight-grams
	   (cons
	    (*
	     (car
	      (any-dimensional-unit-to-grams-dimensional-unit 
	       (get-dimensional-unit-from-upc-attribute
		alist-upc
		(car this-recipe-element)
		'serving-size)))
	     (car
	      (get-dimensional-unit-from-upc-attribute
	       alist-upc
	       (car this-recipe-element)
	       'servings-per-container)))
	    'grams)))
       (cons
	(car this-recipe-element)
	(/
	 (car
	  (any-dimensional-unit-to-grams-dimensional-unit
	   (cdr this-recipe-element)))
	 (car
	  total-container-weight-grams)))))
   list-recipe-by-dimensional-units))

;; the only difference is division or multiplication...
;; TODO : get these ^ two v functions merged into one,
;; write a function #'get-total-container-weight-grams.

;; the same as calculate-recipe-portion-percentages-from-dimensional-units,
;; but a multiplication occurs, instead of a division.
(defun calculate-recipe-dimensional-units-from-portion-percentages
    (alist-upc
     list-recipe-by-percentages)
  ""
  (mapcar
   (lambda
       (this-recipe-element)
     (let
	 ((total-container-weight-grams
	   (cons
	    (*
	     (car
	      (any-dimensional-unit-to-grams-dimensional-unit
	       (get-dimensional-unit-from-upc-attribute
		alist-upc
		(car this-recipe-element) 
		'serving-size)))
	     (car
	      (get-dimensional-unit-from-upc-attribute
	       alist-upc
	       (car this-recipe-element)
	       'servings-per-container)))
	    'grams)))
       (cons
	(car this-recipe-element)
	(cons 
	 (*
	  (cdr this-recipe-element)
	  (car
	   total-container-weight-grams))
	 'grams))))
   list-recipe-by-percentages))

(defun calculate-list-upc-percentage-after-using-upc-leftover
    (alist-upc
     alist-inventory
     alist-role-alias
     alist-upc-leftover
     alist-recipe-leftover
     list-upc-percentages)
  ""
  (mapcar
   (lambda
       (this-upc)
     (cons
      (car this-upc)
      (let
	  ((available-in-upc-leftover
	    (cdr
	     (assoc
	      (car this-upc)
	      alist-upc-leftover
	      :test #'equalp)))
	   (needed-for-recipe
	    (cdr this-upc)))
	(if
	 (numberp available-in-upc-leftover)
	 (progn
	   (format t "~s : ~s~%" (car this-upc) needed-for-recipe)
	   (- needed-for-recipe available-in-upc-leftover))
	 (cdr this-upc)))))
   list-upc-percentages))

;; do not confuse with calculate-cook-recipe-using-upc-leftovers. this function
;; is the 'singular' lower-level function which is invoked by the 'plural'
;; higher-level function, calculate-cook-recipe-using-upc-leftovers.
(defun calculate-cook-recipe-using-upc-leftover
    (alist-upc
     alist-recipe
     alist-inventory
     alist-role-alias
     alist-upc-leftover
     list-upc-percentages)
  ""
  (let
      ((result-list-upc-percentages
	(mapcar
	 (lambda
	     (this-upc)
	   (if
	    (<=
	     (cdr this-upc)
	     (get-upc-leftover-percent-or-zero
	      alist-upc-leftover
	      (car this-upc)))
	    (cons
	     (car this-upc)
	     (calculate-cook-recipe-use-leftover-amount
	      (get-upc-leftover-percent-or-zero
	       alist-upc-leftover
	       (car this-upc))
	      (cdr this-upc)))
	    this-upc))
	 list-upc-percentages))
       (result-alist-upc-leftover
	(mapcar
	 (lambda
	     (this-leftover)
	   (let
	       ((percentage-called-for
		 (cdr
		  (assoc
		   (car this-leftover)
		   list-upc-percentages
		   :test #'equalp))))
	     (if
	      percentage-called-for
	      (cons
	       (car this-leftover)
	       (calculate-cook-recipe-new-leftover-amount
		(get-upc-leftover-percent-or-zero
		 alist-upc-leftover
		 (car this-leftover))
		percentage-called-for))
	      this-leftover)))
	 alist-upc-leftover)))
    (values
     result-list-upc-percentages
     result-alist-upc-leftover)))

(defun calculate-cook-recipe-new-leftover-amount
    (available-percentage-in-leftover
     needed-percentage-for-recipe)
  ""
  (if
   (<= needed-percentage-for-recipe available-percentage-in-leftover)
   (progn
     (format t "new-leftover-amount ~s ~s ~%"
	     available-percentage-in-leftover
	     needed-percentage-for-recipe)
     (- available-percentage-in-leftover needed-percentage-for-recipe))
   0))

(defun calculate-cook-recipe-use-leftover-amount
    (available-percentage-in-leftover
     needed-percentage-for-recipe)
  ""
  (format t "use-leftover-amount ~s ~s ~%"
	  available-percentage-in-leftover
	  needed-percentage-for-recipe)
  (if
   (<= needed-percentage-for-recipe available-percentage-in-leftover)
   needed-percentage-for-recipe
   available-percentage-in-leftover))

;; use #'calculate-subtract-percentage-portion-lists, it's less confusing
(defun calculate-subtract-lists-percentage-portion (y x)
  "Subtract the second argument from the first argument"
  (mapcar
   (lambda
       (a)
     (let ((b (assoc (car a) y :test #'equalp)))
       (if
	b
	(cons
	 (car a)
	 (- (cdr b) (cdr a)))
	)))
   x))

;; this function is very inefficient, it should mapcar over little-list instead
(defun calculate-subtract-percentage-portion-lists (big-list little-list)
  "Subtract the 'little-list' (a UPC percentage-portion list with less items)
from the 'big-list'. Returns 'big-list' including the subtractions due to
matches from 'little-list'. Both lists are '((UPC . percentage)) associations."
  (mapcar
   (lambda
       (a)
     (let
	 ((b
	   (assoc (car a) little-list :test #'equalp)))
       (if
	b
	(cons (car a) (- (cdr a) (cdr b)))
	a)))
   big-list))

(defun calculate-remove-used-leftovers-from-alist-upc-leftover
    (alist-upc
     alist-upc-leftover
     list-upc-percentages)
  ""
  (let
      ((result-alist-upc-leftover
	(mapcar
	 (lambda
	     (this-upc-leftover) ;; for each alist-upc-leftover
	   (let
	       ((matching-leftover
		 (assoc
		  (car this-upc-leftover)
		  list-upc-percentages
		  :test #'equalp)))
	     (if
	      matching-leftover
	      (let
		  ((this-upc-leftover-result
		    (cons
		     (car this-upc-leftover)
		     (-
		      (cdr this-upc-leftover)
		      (cdr matching-leftover)))))
		(if
		 (>
		  (cdr this-upc-leftover-result)
		  0)
		 (list this-upc-leftover-result)))
	      this-upc-leftover)))
	 alist-upc-leftover)))
    (if
     (equalp
      (last result-alist-upc-leftover)
      (list nil))
     (butlast result-alist-upc-leftover)
     result-alist-upc-leftover)))

;; needs to return
;; 'list-upc-percentages-after-reusing-leftover' and
;; 'result-alist-upc-leftover-after-reusing-leftover'
(defun calculate-after-using-leftover
    (alist-upc
     alist-history
     alist-recipe
     alist-inventory
     alist-role-alias
     alist-upc-leftover
     alist-recipe-leftover)
  "Used in prompt-consume-upc-from-upc-leftover-or-inventory"
  
  )

(defun calculate-upc-list-in-leftover-sum-to-total-required
    (alist-upc
     alist-role-alias
     alist-upc-leftover
     recipe-using-role-aliases
     role-alias
     minimum-dimensional-unit
     &optional
       (minimum-dimensional-unit '(1 . grams))
       (new-upc-list '()))
  ""
  (if
   (> (car minimum-dimensional-unit) 0)
   (let
       ((role-alias-filler-upc "123456678"))
     (let
	 ((this-amount-available-in-leftover 0)
	  (this-amount-needed-for-recipe 0))
       (values this-amount-avail)))))

;; menu option 44 and 45...
;; do not confuse with calculate-cook-recipe-using-upc-leftover. this function
;; is the 'plural' higher-level function which invokes the lower-level
;; function calculate-cook-recipe-using-upc-leftover.
(defun calculate-cook-recipe-using-upc-leftovers
    (format-t
     alist-upc
     alist-history
     alist-recipe
     alist-inventory
     alist-role-alias
     alist-upc-leftover
     alist-recipe-leftover
     alist-consumption-cost
     recipe-name
     portion-consumed
     portion-consumed-time)
  "calculate-cook-recipe-using-upc-leftovers"
  (let
      ((user-log-upc-list
	(calculate-merge-and-sum-duplicate-upc-dimensional-units
	 (convert-recipe-role-alias-keys-to-upc-keys
	  alist-upc
	  alist-recipe
	  alist-inventory
	  alist-role-alias
	  (convert-recipe-role-alias-keys-to-upc-keys-use-up-leftover
	   alist-upc
	   alist-recipe
	   alist-inventory
	   alist-role-alias
	   alist-upc-leftover
	   (get-recipe-upc-dimensional-unit-list
	    alist-recipe
	    recipe-name))))))
    (format format-t "user-log-upc-list ~s~%" user-log-upc-list)
    (let
	((result-calculate-cook-recipe-using-upc-leftover
	  (calculate-cook-recipe-using-upc-leftover ;; NOT recursion!!!
	   alist-upc ;; check the note above about plural vs. singular
	   alist-recipe
	   alist-inventory
	   alist-role-alias
	   alist-upc-leftover
	   (calculate-recipe-portion-percentages-from-dimensional-units
	    alist-upc
	    user-log-upc-list)))
	 (result-consume-from-leftover-or-inventory
	  (multiple-value-list
	   (prompt-consume-upc-from-inventory-adjust-consumption-cost
	    format-t
	    alist-upc
	    alist-inventory
	    (car 
	     (calculate-remove-used-leftovers-from-alist-upc-leftover
	      alist-upc
	      alist-upc-leftover
	      (calculate-recipe-portion-percentages-from-dimensional-units
	       alist-upc
	       user-log-upc-list)))
	    alist-consumption-cost
	    (calculate-recipe-portion-percentages-from-dimensional-units
	     alist-upc
	     user-log-upc-list)
	    portion-consumed-time))))
      (let
	  ((result-alist-history
	    (acons
	     portion-consumed-time
	     (calculate-percentage-of-dimensional-units
	      user-log-upc-list
	      portion-consumed)
	     alist-history))
	   (result-alist-inventory
	    (car
	     result-consume-from-leftover-or-inventory))
	   (result-alist-upc-leftover
	    (cadr
	     result-consume-from-leftover-or-inventory))
	   (result-alist-recipe-leftover
	    (if
	     (> (- 1.0 portion-consumed) 0)
	     (acons
	      recipe-name
	      (acons
	       portion-consumed-time
	       (cons
		(- 1.0 portion-consumed)
		user-log-upc-list)
	       (cdr
		(assoc
		 recipe-name
		 alist-recipe-leftover
		 :test #'equalp)))
	      (remove
	       recipe-name
	       alist-recipe-leftover
	       :key #'car :test #'equalp))
	     alist-recipe-leftover))
	   (result-alist-consumption-cost
	    (caddr result-consume-from-leftover-or-inventory)))
	(list
	 result-alist-history
	 result-alist-inventory
	 result-alist-upc-leftover
	 result-alist-recipe-leftover
	 result-alist-consumption-cost)))))

;; get the total of "attribute" which is available in alist-inventory
(defun calculate-available-upc-attribute-from-inventory
    (alist-upc
     alist-inventory
     attribute
     &optional
       (seed-upc "688267020087"))
  "add up all the calories available in inventory"
  (let
      ((seed-attribute-1
	(get-upc-attribute alist-upc seed-upc attribute)))
    (let
	((seed-attribute-2
	  (if (numberp (car seed-attribute-1))
	      (cdr seed-attribute-1)
	      (cdar seed-attribute-1))))
      (cons 
       (apply
	#'+
	(mapcar
	 (lambda
	     (this-upc)
	   (let
	       ((this-upc-available-quantity
		 (get-inventory-total-quantity alist-inventory (car this-upc))))
	     (if
	      (> this-upc-available-quantity  0)
	      (let
		  ((this-upc-attribute
		    (get-upc-attribute alist-upc (car this-upc) attribute)))
		(if
		 this-upc-attribute 
		 (*
		  this-upc-available-quantity
		  (car this-upc-attribute)
		  (car
		   (get-upc-attribute
		    alist-upc
		    (car this-upc)
		    'servings-per-container)))
		 0))
	      0)))
	 alist-upc))
       seed-attribute-2))))

;; given a upc list of dimensional units,
;; merge any duplicates, summing their totals.
;; this is NOT for upc percent portion lists.
(defun calculate-merge-and-sum-duplicate-upc-dimensional-units
    (upc-list
     &optional
       (result-upc-list '()))
  "recursive list eater function to merge and sum duplicate UPC's."
  (if
   upc-list
   (let
       ((found-duplicate
	 (assoc (caar upc-list) result-upc-list :test #'equalp)))
     (if
      found-duplicate
      (let
	  ((actual-result
	    (cons
	     (caar upc-list)
	     (add-two-dimensional-units
	      (cdar upc-list)
	      (cdr found-duplicate)))))
	(calculate-merge-and-sum-duplicate-upc-dimensional-units
	 (cdr upc-list)
	 (append
	  (remove
	   found-duplicate
	   result-upc-list
	   :test #'equalp)
	  (list actual-result))))
      (calculate-merge-and-sum-duplicate-upc-dimensional-units
       (cdr upc-list)
       (append result-upc-list (list (car upc-list))))))
   result-upc-list))

(defun calculate-attribute-total-per-upc-unit
    (alist-upc
     user-upc
     user-attribute
     user-upc-price)
  (cons
   (*
    (car
     (any-dimensional-unit-to-grams-dimensional-unit
      (get-upc-attribute
       alist-upc
       user-upc
       user-attribute)))
    (car
     (get-upc-attribute
      alist-upc
      user-upc
      'servings-per-container)))
   'grams))

(defun calculate-attribute-per-cent
    (alist-upc
     user-upc
     user-attribute
     user-upc-price)
  ""
  (let
      ((total-of-attribute-in-upc-unit
	(calculate-attribute-total-per-upc-unit
	 alist-upc
	 user-upc
	 user-attribute
	 user-upc-price)))
    (let
	((result-attribute-per-cent
	  (/ 
	   (/ (car total-of-attribute-in-upc-unit)
	      user-upc-price) ;; multiplying user-upc-price by 100 
	   100.0)))           ;; might increase readability
      result-attribute-per-cent)))

;; calculate the exact cost of a recipe down to the individual units
(defun calculate-recipe-total-cost
    (alist-upc
     alist-recipe
     alist-inventory
     alist-role-alias
     user-recipe)
  ""
  (let
      ((user-log-upc-list
	(convert-recipe-role-alias-keys-to-upc-keys
	 alist-upc
	 alist-recipe
	 alist-inventory
	 alist-role-alias
	 (get-recipe-upc-dimensional-unit-list
	  alist-recipe
	  user-recipe))))
    (let
	((user-log-upc-percent-list
	  (calculate-recipe-portion-percentages-from-dimensional-units
	   alist-upc
	   user-log-upc-list))
	 (user-log-upc-price-list
	  (mapcar
	   (lambda (this-upc-in-the-recipe)
	     (cons
	      (car this-upc-in-the-recipe)
	      (read-from-string
	       (get-inventory-last-price-group
		alist-inventory
		(car this-upc-in-the-recipe)
		(get-inventory-last-date-group
		 alist-inventory
		 (car this-upc-in-the-recipe))))))
	   user-log-upc-list)))
      (apply
       #'+
       (mapcar
	(lambda
	    (this-upc)
	  (*
	   (cdr this-upc)
	   (cdr
	    (assoc
	     (car this-upc)
	     user-log-upc-percent-list
	     :test #'equalp))))
	user-log-upc-price-list)))))

;; calculate cost of a list of whole-unit UPCs (imagine not using leftovers) 
(defun calculate-recipe-procurement-cost
    (alist-upc
     alist-recipe
     alist-inventory
     alist-role-alias
     user-recipe)
  ""
  (let
      ((user-log-upc-list
	(convert-recipe-role-alias-keys-to-upc-keys
	 alist-upc
	 alist-recipe
	 alist-inventory
	 alist-role-alias
	 (get-recipe-upc-dimensional-unit-list
	  alist-recipe
	  user-recipe))))
    (let
	((user-log-upc-price-list
	  (mapcar
	   (lambda (this-upc-in-the-recipe)
	     (cons
	      (car this-upc-in-the-recipe)
	      (read-from-string
	       (get-inventory-last-price-group
		alist-inventory
		(car this-upc-in-the-recipe)
		(get-inventory-last-date-group
		 alist-inventory
		 (car this-upc-in-the-recipe))))))
	   user-log-upc-list)))
      (apply
       #'+
       (mapcar
	(lambda
	    (this-upc)
	  (cdr this-upc))
	user-log-upc-price-list)))))

;; returns 3 values: the attribute-per-cent,
;; the total-attribute-in-recipe and the total-cost-of-recipe.
;; NOTE old version, not used, see the definition without the % in the name
(defun calculate-recipe-attribute-per-cent%
    (alist-upc
     alist-recipe
     alist-inventory
     alist-role-alias
     user-recipe
     user-attribute)
  ""
  (let
      ((user-log-upc-list
	(convert-recipe-role-alias-keys-to-upc-keys
	 alist-upc
	 alist-recipe
	 alist-inventory
	 alist-role-alias
	 (get-recipe-upc-dimensional-unit-list
	  alist-recipe
	  user-recipe))))
    (let
	((user-log-upc-percent-list
	  (calculate-recipe-portion-percentages-from-dimensional-units
	   alist-upc
	   user-log-upc-list))
	 (user-log-upc-price-list
	  (mapcar
	   (lambda (this-upc-in-the-recipe)
	     (cons
	      (car this-upc-in-the-recipe)
	      (read-from-string
	       (get-inventory-last-price-group
		alist-inventory
		(car this-upc-in-the-recipe)
		(get-inventory-last-date-group
		 alist-inventory
		 (car this-upc-in-the-recipe))))))
	   user-log-upc-list)))
      (let
	  ((user-log-upc-attribute-per-unit-list
	    (mapcar
	     (lambda (this-upc-in-the-recipe)
	       (cons
		(car this-upc-in-the-recipe)
		(calculate-attribute-total-per-upc-unit
		 alist-upc
		 (car this-upc-in-the-recipe)
		 user-attribute
		 (cdr
		  (assoc
		   (car this-upc-in-the-recipe)
		   user-log-upc-price-list
		   :test #'equalp)))))
	     user-log-upc-list)))
	(let
	    ((total-attribute-in-recipe
	      (apply
	       #'+
	       (mapcar
		(lambda
		    (this-upc)
		  (*
		   (cadr this-upc)
		   (cdr
		    (assoc
		     (car this-upc)
		     user-log-upc-percent-list
		     :test #'equalp))))
		user-log-upc-attribute-per-unit-list)))
	     (total-cost-of-recipe
	      (apply
	       #'+
	       (mapcar
		(lambda
		    (this-upc)
		  (*
		   (cdr this-upc)
		   (cdr
		    (assoc
		     (car this-upc)
		     user-log-upc-percent-list
		     :test #'equalp))))
		user-log-upc-price-list))))
	  (values
	   (/ (/ total-attribute-in-recipe total-cost-of-recipe) 100.0)
	   total-attribute-in-recipe
	   total-cost-of-recipe))))))

;; returns 3 values: the attribute-per-cent,
;; the total-attribute-in-recipe and the total-cost-of-recipe.  
(defun calculate-recipe-attribute-per-cent
    (alist-upc
     alist-recipe
     alist-inventory
     alist-role-alias
     user-recipe
     user-attribute)
  ""
  (let
      ((user-log-upc-list
	(convert-recipe-role-alias-keys-to-upc-keys
	 alist-upc
	 alist-recipe
	 alist-inventory
	 alist-role-alias
	 (get-recipe-upc-dimensional-unit-list
	  alist-recipe
	  user-recipe))))
    (calculate-upc-list-attribute-percent
     alist-upc
     alist-inventory
     user-log-upc-list
     user-attribute)))

;; used by calculate-recipe-attribute-per-cent AND option 63
(defun calculate-upc-list-attribute-percent
    (alist-upc
     alist-inventory
     user-log-upc-list
     user-attribute)
  ""
  (let
      ((user-log-upc-percent-list
	(calculate-recipe-portion-percentages-from-dimensional-units
	 alist-upc
	 user-log-upc-list))
       (user-log-upc-price-list
	(mapcar
	 (lambda (this-upc-in-the-upc-list)
	   (cons
	    (car this-upc-in-the-upc-list)
	    (read-from-string
	     (get-inventory-last-price-group
	      alist-inventory
	      (car this-upc-in-the-upc-list)
	      (get-inventory-last-date-group
	       alist-inventory
	       (car this-upc-in-the-upc-list))))))
	 user-log-upc-list)))
    (let	
	((user-log-upc-attribute-per-unit-list
	  (mapcar
	   (lambda (this-upc-in-the-upc-list)
	     (cons
	      (car this-upc-in-the-upc-list)
	      (calculate-attribute-total-per-upc-unit
	       alist-upc
	       (car this-upc-in-the-upc-list)
	       user-attribute
	       (cdr
		(assoc
		 (car this-upc-in-the-upc-list)
		 user-log-upc-price-list
		 :test #'equalp)))))
	   user-log-upc-list)))
      (let
	  ((total-attribute-in-upc-list
	    (apply
	     #'+
	     (mapcar
	      (lambda
		  (this-upc)
		(*
		 (cadr this-upc)
		 (cdr
		  (assoc
		   (car this-upc)
		   user-log-upc-percent-list
		   :test #'equalp))))
	      user-log-upc-attribute-per-unit-list)))
	   (total-cost-of-upc-list
	    (apply
	     #'+
	     (mapcar
	      (lambda
		  (this-upc)
		(*
		 (cdr this-upc)
		 (cdr
		  (assoc
		   (car this-upc)
		   user-log-upc-percent-list
		   :test #'equalp))))
	      user-log-upc-price-list))))
	(values
	 (/ (/ total-attribute-in-upc-list total-cost-of-upc-list) 100.0)
	 total-attribute-in-upc-list
	 total-cost-of-upc-list)))))

;; -----------------------------------------------------------------------------
;; the following definition(s) pertain to formatting data for user digestion.

;; from http://cl-cookbook.sourceforge.net/dates_and_times.html
(defun format-universal-seconds (format-t user-universal-seconds time-zone)
  ""
  (let ((*day-names*
	 '("Monday" "Tuesday" "Wednesday"
	   "Thursday" "Friday" "Saturday"
	   "Sunday"))
	(*month-names*
	 '("January" "Feburary" "March" "April" "May" "June" "July"
	   "August" "September" "October" "November" "December")))
    (multiple-value-bind
	  (second minute hour date month year day-of-week dst-p tz)
	(decode-universal-time user-universal-seconds time-zone)
      (format
       format-t
       "~d is ~2,'0d:~2,'0d:~2,'0d on ~a, ~a/~2,'0d/~d (GMT~@d)"
       user-universal-seconds
       hour
       minute
       second
       (nth day-of-week *day-names*)
       (nth (- month 1) *month-names*)
       date
       year
       tz))))



;; the heart of foodstats. list-foodstats is a list of attributes which  
;; ought to be found in each UPC.
;; format-t is provided to redirect output somewhere else.
;; TODO a better textual layout is possible.
;; TODO how to handle when certain UPC's don't have a requested attribute from
;; list-foodstats
(defun format-nutrition-report
    (format-t
     alist-upc
     alist-inventory
     alist-role-alias
     list-recipe
     list-foodstats
     portion-percentage)
  "Format (print) a report of the Nutrition Facts, given list-recipe-upcs,
list-foodstats, and the portion-percentage [0.0, 1.0].
The list-foodstats are the attributes from each UPC.
The argument format-t is passed as the first argument to format.
Returns T when the function completes."
  (progn 
    (format format-t "~%Nutrition Facts for a recipe containing:~%~%")
    (mapcar
     #'(lambda
	   (upc)
	 (let
	     ((upc-from-alias
	       (get-role-alias-upc-has-inventory
		alist-upc
		alist-inventory
		alist-role-alias
		(car upc))))
	   (format
	    format-t
	    "- ~s: ~s~%"
	    (if
	     upc-from-alias
	     upc-from-alias
	     (car upc))
	    
	    (get-upc-attribute
	     alist-upc	       
	     (if
	      upc-from-alias
	      upc-from-alias
	      (car upc))
	     'name))))
     list-recipe)
    (format
     format-t
     "~%Report for eating ~3,2f% of the recipe output:~%~%"
     (* 100 portion-percentage))
    (let
	((calculation-results
	  (calculate-nutrition-report-from-recipe-by-portion-percentages
	   alist-upc
	   list-recipe
	   list-foodstats
	   portion-percentage)))
      (mapcar
       #'(lambda
	     (nutrition-fact)
	   (if
	    (assoc nutrition-fact calculation-results) 
	    (format
	     format-t
	     "Total ~s consumed:~%                             ~9,2f ~s~%"
	     (symbol-name nutrition-fact)
	     (cadr
	      (assoc nutrition-fact calculation-results))
	     (symbol-name
	      (cddr
	       (assoc nutrition-fact calculation-results))))
	    nil))
       list-foodstats))
    T))

;; -----------------------------------------------------------------------------
;; the following definitions pertain to disk I/O of association lists.

;; serialize an association list to disk
(defun sexp-serialize (alist file)
  "Write the S-expression to disk at file. Returns T when complete."
  (with-open-file (stream file :direction :output)
    (format stream "~s~%" alist))
  T)

;; from disk, de-serialize into memory, an association list 
(defun sexp-deserialize (file)
  "Read the S-expression, stored on disk in file.
Uses read. Returns an S-expression. If file not found, returns an empty list."
  (handler-case
      (with-open-file (stream file)
	(read stream))
    (error (e)
      (format t "..new ~s.." file)
      '())))

;; -----------------------------------------------------------------------------
;; the following definitions pertain to the user controlling the model
;; through application specific Read Eval Print Loops.
;; higher order program flow control will be defined in a different
;; section. these definitions are lower level.

;; read a dimensional unit from the user, returns a cons cell.
(defun prompt-dimensional-unit-pair (format-t tooltip-string)
  "Have the user enter a dimensional unit pair.
Uses read-from-string. Returns a cons cell."
  (progn
    (format format-t "~s~%" tooltip-string)
    (format format-t "Enter a scalar value (for example, 3): ")
    (let ((entered-scalar (read-from-string (read-line))))
      (format format-t "Enter a unit value (for example, grams): ")
      (let ((entered-unit (read-from-string (read-line))))
	(cons entered-scalar entered-unit)))))

;; read a string
(defun prompt-string (format-t tooltip-string)
  (progn
    (format format-t "~s: " tooltip-string)
    (let ((entered-value (read-line)))
      entered-value)))

;; read a symbol or floating point number
(defun prompt-symbol-or-float (format-t tooltip-string)
  "Have the user enter a floating point numer, or a symbol.
Uses read-from-string. Returns a fixed value."
  (progn
    (format format-t "~s: " tooltip-string)
    (let ((entered-value (read-from-string (read-line))))
      entered-value)))

;; read an integer
(defun prompt-integer (format-t tooltip-string)
  "Have the user enter an integer. Uses parse-integer. Returns an integer."
  (progn
    (format format-t "~s: " tooltip-string)
    (let ((entered-value (parse-integer (read-line))))
      entered-value)))

;; when invoked, ask questions to guide the user through
;; adding/updating a UPC and a single attribute
(defun prompt-upc-attributes (format-t alist-upc)
  "User interface to read in an attribute for a UPC. 
Returns the new association list containing the new information."
  (progn
    (let
	((target-upc
	  (prompt-string
	   format-t
	   "Adjusting the attributes of which UPC")))
      (let
	  ((target-attribute
	    (prompt-symbol-or-float
	     format-t
	     "Adjusting which attribute")))
	(if (or (eq target-attribute 'name)
		(eq target-attribute 'ingredients))
	    (progn ;; first case, if the attribute is a simple string
	      (adjust-alist-upc-attribute
	       alist-upc
	       target-upc
	       (cons
		target-attribute
		(prompt-string format-t "Enter the desired value"))))
	    (progn ;; second case, if the attribute is a dimensional-unit
	      (adjust-alist-upc-attribute
	       alist-upc
	       target-upc
	       (cons
		target-attribute
		(cons
		 (prompt-integer
		  format-t
		  "Enter a scalar integer (for example, 3)")
		 (prompt-symbol-or-float
		  format-t
		  "Enter a unit (for example, grams)"))))))))))

(defun prompt-upc-list-by-dimensional-unit
    (format-t alist-upc &optional (result-upc-du-list '()))
  "Prompt the user for a UPC list and have them enter a quantity for each.
Returns a list of what the user entered."
  (let
      ((read-in-upc
	(prompt-string
	 format-t
	 "Press '0' to continue, else, add a UPC to the list")))
    (if
     (or
      (equalp read-in-upc "0") ;; base case
      (equalp read-in-upc 0))
     result-upc-du-list            ;; return the built-up list
     (let 
	 ((read-in-dimensional-unit
	   (prompt-dimensional-unit-pair
	    format-t
	    "What quantity of this ingredient for the list?")))
       (prompt-upc-list-by-dimensional-unit ;; recursive call to build upc list
	format-t      
	alist-upc
	(append       ;; add the existing list to a new list with new data
	 result-upc-du-list   
	 (list           ;; car is upc and cdr is a dimensional-unit cons.
	  (cons read-in-upc read-in-dimensional-unit))))))))

(defun prompt-upc-list-by-percentage
    (format-t alist-upc &optional (result-upc-percent-list '()))
  "Prompt the user for a UPC list and have them enter a quantity for each.
The list will be a pair of UPC's to percentage of that UPC consumed.
Returns a list of these pairs."
  (let
      ((read-in-upc
	(prompt-string
	 format-t
	 "Press '0' to continue, else, add a UPC to the list")))
    (if
     (or
      (equalp read-in-upc "0") ;; base case
      (equalp read-in-upc 0))
     result-upc-percent-list ;; return the built-up list
     (let
	 ((read-in-percentage
	   (prompt-percentage
	    format-t
	    "What percentage did you consume? No decimal [0,100+")))
       (prompt-upc-list-by-percentage ;; recurisve call to build upc list
	format-t
	alist-upc
	(append ;; add the existing list to a new list with new data
	 result-upc-percent-list
	 (list
	  (cons read-in-upc read-in-percentage))))))))

;; read a symbol list from the user
(defun prompt-symbol-list
    (format-t alist-upc &optional (result-attributes '()))
  "Ask for a list of attributes to include in the report."
  (format
   format-t
   "Press '0' to continue, else, add an attribute to the nutrition report: ")
  (let
      ((read-in-value (read-line)))
    (if (equalp read-in-value "0")
	result-attributes
	(prompt-symbol-list
	 format-t alist-upc
	 (append
	  result-attributes
	  (list (read-from-string read-in-value)))))))

;; make the user type the word "percentage" or "dimensional unit"
(defun prompt-percentage-or-dimensional-unit
    (format-t
     &optional (prompt "Use percentage or dimensional unit?~%"))
  "Ask whether the user wants to use percentage of a UPC or to type in the
exact dimensional unit of the UPC they had consumed.
Returns either string, 'percentage' or 'dimensional unit'
(this increases code readability)."
  (format
   format-t
   "Type 'percentage' or 'dimensional unit', and press enter: " prompt)
  (let
      ((read-in-value (read-line)))
    (if
     (not
      (or
       (equalp read-in-value "dimensional unit")
       (equalp read-in-value "percentage")))
     (progn
       (format format-t "No, please try again...~%")
       (prompt-percentage-or-dimensional-unit format-t))
     read-in-value)))

(defun prompt-percentage
    (format-t
     &optional
       (prompt
	"What percentage of this did you intent to consume? [0, 100+ "))
  "Ask a simple question, where the answer is an integer [0, 100]"
  (format format-t "~s" prompt)
  (/ (parse-integer (read-line)) 100.0))

(defun prompt-upc-list (format-t &optional (result-upc '()))
  "Read a list of UPC's from a user to add to their log."
  (format
   format-t
   "Press '0' to continue, else, add a recently consumed UPC: ")
  (let ((read-in-value (read-line)))
    (if (equalp read-in-value "0")
	result-upc
	(prompt-upc-list
	 format-t
	 (append
	  result-upc
	  (list read-in-value))))))

(defun prompt-time-range (format-t tooltip-string)
  ""
  (format format-t tooltip-string)
  (let
      ((start-time
	(prompt-timestamp
	 format-t
	 "Enter the start time~%"))
       (end-time
	(prompt-timestamp
	 format-t
	 "Enter the end time~%")))
    (cons start-time end-time)))

(defun prompt-container-weight-and-serving-size (format-t)
  (format
   format-t
   "This prompt will calculate 'servings-per-container',
given the container weight, and the serving size.~%~%")
  (let
      ((container-weight
	(prompt-dimensional-unit-pair
	 format-t
	 "Enter the weight of the full container"))
       (serving-size
	(prompt-dimensional-unit-pair
	 format-t
	 "Enter the serving size listed on the Nutrition Facts")))
    (let
	((result
	  (calculate-servings-per-container-from-container-weight
	   serving-size
	   container-weight)))
      (format
       format-t
       "The container's servings-per-container attribute is:~%~s~%" result)
      result)))

(defun prompt-inventory-adjustment (format-t alist-upc alist-inventory)
  "Add or update the quantity of upc for a specific date group.
Prompts the user through the process"
  (let
      ((upc
	(prompt-string
	 format-t
	 "Enter the UPC"))
       (date
	(prompt-string
	 format-t
	 "Enter the Expiration Date (19-AUG-1991 for example)"))
       (price
	(prompt-string
	 format-t
	 "Enter the price, omit the currency symbol"))
       (quantity
	(prompt-integer
	 format-t
	 "What integer quantity is in this group?")))
    (let
	((result
	  (adjust-alist-inventory
	   alist-inventory upc date price quantity)))
      result)))

(defun prompt-role-upc-assign
    (format-t
     alist-upc
     alist-role-alias)
  ""
  (let
      ((upc
	(prompt-string
	 format-t
	 "Enter a UPC"))
       (role-alias
	(prompt-string
	 format-t
	 "Enter a role alias to use in recipes")))
    (let
	((result
	  (adjust-alist-role-alias
	   alist-role-alias upc role-alias)))
      result)))

(defun prompt-consume-upc-from-inventory
    (format-t
     alist-upc
     alist-inventory
     alist-upc-leftover
     &optional
       (list-upc-percentages
	(if
	 (equalp "percentage"
		 (prompt-percentage-or-dimensional-unit format-t alist-upc))
	 (prompt-upc-list-by-percentage format-t)
	 (calculate-recipe-portion-percentages-from-dimensional-units
	  alist-upc
	  (prompt-upc-list-by-dimensional-unit format-t alist-upc))))
       (list-upc-overages '()))
  ""
  (if
   list-upc-percentages
   (let 
       ((result-alist-inventory
	 (consume-from-alist-inventory
	  alist-upc
	  alist-inventory
	  (caar list-upc-percentages)
	  (cdar list-upc-percentages))))
     (if
      (numberp result-alist-inventory)
      (prompt-consume-upc-from-inventory ;; overage, do not adjust inventory
       format-t
       alist-upc
       alist-inventory ;; do not modify inventory because there was an overage
       alist-upc-leftover
       (cdr ;; list-eater function
	list-upc-percentages)
       (append
	list-upc-overages
	(list
	 (cons
	  (caar
	   list-upc-percentages)
	  (cdar
	   list-upc-percentages)))))
      (let
	  ((inventory-deduction
	    (ceiling
	     (cdar list-upc-percentages)))
	   (leftover-percentage
	    (* -1 (nth-value 1 (ceiling (cdar list-upc-percentages))))))
	(prompt-consume-upc-from-inventory ;; no overage, adjust inventory
	 format-t
	 alist-upc
	 (consume-from-alist-inventory ;; adjust the inventory
	  alist-upc
	  alist-inventory
	  (caar list-upc-percentages)
	  inventory-deduction)
	 (if ;; update alist-upc-leftover if the user consumed less than 100%
	  (> leftover-percentage 0.0)
	  (append
	   (remove
	    (caar list-upc-percentages)
	    alist-upc-leftover
	    :test #'equalp :key #'car)
	   (list
	    (cons
	     (caar list-upc-percentages)
	     (+
	      leftover-percentage
	      (get-upc-leftover-percent-or-zero
	       alist-upc-leftover
	       (caar list-upc-percentages))))))
	  alist-upc-leftover)
	 (cdr ;; list-eater function
	  list-upc-percentages)
	 list-upc-overages))))
   (values
    alist-inventory
    alist-upc-leftover
    list-upc-overages)))

;; same thing as prompt-consume-upc-from-inventory but handles consumption cost
(defun prompt-consume-upc-from-inventory-adjust-consumption-cost
    (format-t
     alist-upc
     alist-inventory
     alist-upc-leftover
     alist-consumption-cost
     &optional
       (list-upc-percentages
	(if
	 (equalp "percentage"
		 (prompt-percentage-or-dimensional-unit format-t alist-upc))
	 (prompt-upc-list-by-percentage format-t)
	 (calculate-recipe-portion-percentages-from-dimensional-units
	  alist-upc
	  (prompt-upc-list-by-dimensional-unit format-t alist-upc))))
       (consumption-time (get-universal-time))
       (list-upc-overages '()))
  "this is prompt-consume-upc-from-inventory upgraded for consumption costs."
  (if
   list-upc-percentages
   (let
       ((parent-result
	 (multiple-value-list
	  (consume-from-alist-inventory-adjust-consumption-cost
	   alist-upc
	   alist-inventory
	   alist-consumption-cost
	   (caar list-upc-percentages)
	   (cdar list-upc-percentages)
	   consumption-time))))
     (let
	 ((result-alist-inventory
	   (car parent-result))
	  (result-alist-consumption-cost
	   (cadr parent-result)))
       (if
	(numberp result-alist-inventory)
	(prompt-consume-upc-from-inventory-adjust-consumption-cost
	 format-t
	 alist-upc
	 alist-inventory
	 alist-upc-leftover
	 alist-consumption-cost
	 (cdr list-upc-percentages)
	 consumption-time
	 (append
	  list-upc-overages
	  (list
	   (cons
	    (caar
	     list-upc-percentages)
	    (cdar
	     list-upc-percentages)))))
	(let
	    ((inventory-deduction
	      (ceiling
	       (cdar list-upc-percentages)))
	     (leftover-percentage
	      (* -1 (nth-value 1 (ceiling (cdar list-upc-percentages))))))
	  (format format-t "inventory-deduction ~d~%" inventory-deduction)
	  (format format-t "leftover-percentage ~d~%" leftover-percentage)
	  (let
	      ((local-result
		(multiple-value-list
		 (consume-from-alist-inventory-adjust-consumption-cost
		  alist-upc
		  alist-inventory
		  alist-consumption-cost
		  (caar list-upc-percentages)
		  inventory-deduction
		  consumption-time))))
	    (prompt-consume-upc-from-inventory-adjust-consumption-cost
	     format-t
	     alist-upc
	     (car local-result)
	     (if ;; update leftovers if the user consumed less than 100%
	      (> leftover-percentage 0.0)
	      (append
	       (remove
		(caar list-upc-percentages)
		alist-upc-leftover
		:test #'equalp :key #'car)
	       (list
		(cons
		 (caar list-upc-percentages)
		 (+
		  leftover-percentage
		  (get-upc-leftover-percent-or-zero
		   alist-upc-leftover
		   (caar list-upc-percentages))))))
	      alist-upc-leftover)
	     (cadr local-result)
	     (cdr list-upc-percentages)
	     consumption-time
	     list-upc-overages))))))
   (values
    alist-inventory
    alist-upc-leftover
    alist-consumption-cost
    list-upc-overages)))

(defun prompt-consume-upc-from-upc-leftover-or-inventory
    (format-t
     alist-upc
     alist-inventory
     alist-upc-leftover
     &optional
       (list-upc-percentages
	(if
	 (equalp "percentage"
		 (prompt-percentage-or-dimensional-unit format-t alist-upc))
	 (prompt-upc-list-by-percentage format-t)
	 (calculate-recipe-portion-percentages-from-dimensional-units
	  alist-upc
	  (prompt-upc-list-by-dimensional-unit format-t alist-upc))))
       (list-upc-overages '()))
  "prompt-consume-upc-from-upc-leftover-or-inventory"
  (if
   list-upc-percentages
   (let
       ((list-upc-percentages-after-reusing-leftover ;; already done...
	 list-upc-percentages)
	(result-alist-upc-leftover-after-reusing-leftover
	 (calculate-remove-used-leftovers-from-alist-upc-leftover
	  alist-upc
	  alist-upc-leftover
	  list-upc-percentages)))
     (let 
	 ((result-alist-inventory
	   (consume-from-alist-inventory
	    alist-upc
	    alist-inventory
	    (caar list-upc-percentages-after-reusing-leftover)
	    (cdar list-upc-percentages-after-reusing-leftover))))
       (if
	(numberp result-alist-inventory)
	(prompt-consume-upc-from-upc-leftover-or-inventory ;; overage
	 format-t
	 alist-upc
	 alist-inventory ;; do not modify inventory because there was an overage
	 alist-upc-leftover
	 (cdr ;; list-eater function
	  list-upc-percentages-after-reusing-leftover)
	 (append
	  list-upc-overages
	  (list
	   (cons
	    (caar
	     list-upc-percentages-after-reusing-leftover)
	    (cdar
	     list-upc-percentages-after-reusing-leftover)))))
	(let
	    ((inventory-deduction
	      (ceiling
	       (cdar list-upc-percentages-after-reusing-leftover)))
	     (leftover-percentage
	      (* -1 (nth-value 1 (ceiling (cdar list-upc-percentages-after-reusing-leftover))))))
	  (prompt-consume-upc-from-upc-leftover-or-inventory ;; no overage
	   format-t
	   alist-upc
	   (consume-from-alist-inventory ;; adjust the inventory
	    alist-upc
	    alist-inventory
	    (caar list-upc-percentages-after-reusing-leftover)
	    inventory-deduction)
	   (if ;; update alist-upc-leftover if the user consumed less than 100%
	    (> leftover-percentage 0.0)
	    (append
	     (remove
	      (caar list-upc-percentages-after-reusing-leftover)
	      alist-upc-leftover
	      :test #'equalp :key #'car)
	     (list
	      (cons
	       (caar list-upc-percentages-after-reusing-leftover)
	       (+
		leftover-percentage
		(get-upc-leftover-percent-or-zero
		 alist-upc-leftover
		 (caar list-upc-percentages-after-reusing-leftover))))))
	    alist-upc-leftover)
	   (cdr ;; list-eater function
	    list-upc-percentages-after-reusing-leftover)
	   list-upc-overages)))))
   (values
    alist-inventory
    alist-upc-leftover
    list-upc-overages)))

(defun prompt-consume-upc-from-leftover
    (format-t
     alist-upc
     alist-upc-leftover
     &optional
       (list-upc-percentages
	(if
	 (equalp "percentage"
		 (prompt-percentage-or-dimensional-unit format-t))
	 (prompt-upc-list-by-percentage format-t alist-upc)
	 (calculate-recipe-portion-percentages-from-dimensional-units
	  alist-upc
	  (prompt-upc-list-by-dimensional-unit format-t alist-upc))))
       (list-upc-overages '()))
  ""
  (if
   list-upc-percentages
   (let
       ((result-alist-upc-leftover
	 (let
	     ((this-available-percentage
	       (cdr
		(assoc
		 (caar list-upc-percentages)
		 alist-upc-leftover
		 :test #'equalp)))
	      (this-requested-percentage
	       (cdar list-upc-percentages)))
	   (if
	    (and this-requested-percentage this-available-percentage)
	    (if
	     (<= this-requested-percentage this-available-percentage)
	     (adjust-alist-percentage-list
	      alist-upc-leftover
	      (caar list-upc-percentages)
	      (- this-available-percentage this-requested-percentage))
	     (progn
	       (format
		format-t
		"
For UPC on table ~s, you asked for ~d but there is only ~d! No adjustment.~%"
		(caar list-upc-percentages)
		this-requested-percentage
		this-available-percentage)
	       alist-upc-leftover))
	    (format
	     format-t
	     "The UPC ~s is not on the table."
	     (caar list-upc-percentages))))))
     (prompt-consume-upc-from-leftover
      format-t
      alist-upc
      result-alist-upc-leftover
      (cdr list-upc-percentages)))
   alist-upc-leftover))

;; menu option 61
(defun prompt-inventory-attribute-report
    (format-t
     alist-upc
     alist-history
     alist-inventory
     &optional
       (print-result nil)
       (attribute
	(prompt-symbol-or-float
	 format-t
	 "Enter an attribute to run this calculation for"))
       (period-name
	(prompt-string
	 format-t
	 "Enter what the period is (days, hours, etc.)"))
       (quantity-of-attribute-per-period
	(prompt-dimensional-unit-pair
	 format-t
	 "What amount of the attribute will you require per period?")))
  ""
  (let
      ((total-attribute-available
	(calculate-available-upc-attribute-from-inventory
	 alist-upc
	 alist-inventory
	 attribute)))
    (let
	((periods-calculated
	  (cons
	   (/
	    (car (any-dimensional-unit-to-grams-dimensional-unit
		  total-attribute-available))
	    (car (any-dimensional-unit-to-grams-dimensional-unit
		  quantity-of-attribute-per-period)))
	   period-name)))
      (if
       print-result
       (format
	format-t
	"
If consumed at ~d ~s per ~s, 
then inventory has ~d ~s supply of ~s. ~%"
	(car quantity-of-attribute-per-period)
	(cdr quantity-of-attribute-per-period)
	(subseq period-name 0 (- (length period-name) 1))
	(car periods-calculated)
	(cdr periods-calculated)
	attribute)
       periods-calculated))))

(defun prompt-timestamp (format-t tooltip-string)
  (let
      ((user-seconds
	(prompt-integer format-t "Enter the seconds"))
       (user-minutes
	(prompt-integer format-t "Enter the minutes"))
       (user-hours
	(prompt-integer format-t "Enter the hours (24 hour format)"))
       (user-date
	(prompt-integer format-t "Enter the date (day number)"))
       (user-month
	(prompt-integer format-t "Enter the month number (1 through 12)"))
       (user-year
	(prompt-integer format-t "Enter the year (4 digit number)"))
       (user-timezone
	(prompt-integer format-t "Enter the timezone (use - for negative)")))
    (encode-universal-time
     user-seconds
     user-minutes
     user-hours
     user-date
     user-month
     user-year
     user-timezone)))

;; -----------------------------------------------------------------------------
;; the following definitions pertain to high level control.
;; for example, the user-interface entrypoint #'foodstats is defined here.

;; the data file locations. default arguments for #'foodstats-loader
(defparameter +alist-upc-file+ "data/alist-upc.lisp")
(defparameter +alist-history-file+ "data/alist-history.lisp")
(defparameter +alist-recipe-file+ "data/alist-recipe.lisp")
(defparameter +alist-inventory-file+ "data/alist-inventory.lisp")
(defparameter +alist-role-alias-file+ "data/alist-role-alias.lisp")
(defparameter +alist-upc-leftover-file+ "data/alist-upc-leftover.lisp")
(defparameter +alist-recipe-leftover-file+ "data/alist-recipe-leftover.lisp")
(defparameter +alist-consumption-cost-file+ "data/alist-consumption-cost.lisp")

;; the menu prompt string for #'foodstats-console
(defparameter +format-string-main-menu+
  "~%Select a number from the menu:~%
+ To calculate 'servings-per-container' to help with menu option 02, press 01.
+ To add or update a UPC's database attributes, press 02.
+ To view a UPC's database attributes, press 03.
+ To add or update the inventory status of a UPC, press 04.
+ To view the inventory status of a UPC, press 05.
+ For the meat/deli wizard, press 06.

+ To calculate Nutrition Facts without recipes or inventory, press 11.
+ To Log consumption of UPCs and update inventory, press 12.
+ To view leftover UPCs from incomplete consumption, press 13 or 97.
+ To Log consumption of a leftover UPC (from 13), press 14.

+ To add or update a role-alias for a UPC to use in a recipe, press 31.
+ To add or update a named recipe, press 32.

+ To calculate recipe Nutrition Facts without consuming inventory, press 41.
+ To simulate cooking a recipe using only new inventory, press 42.
+ To 'cook' a recipe using only new inventory, press 43.
+ To simulate cooking a recipe using leftovers first, press 44.
+ To 'cook' a recipe using leftover UPCs first, new inventory second, press 45.
+ To view leftover recipes from incomplete consumption, press 46 or 96.
+ To log consumption of leftover recipes, press 47.
+ To calculate recipe cost (of exact portions used in a recipe), press 48.
+ To calculate procurement cost (of needed UPCs to facilitate recipe), press 49.

+ To generate Nutrition Facts between two points in time, press 51.
+ To generate a cost consumption report between two points in time, press 52.
+ To convert a universal seconds time stamp to human readable, press 53.
+ To convert a human readable timestamp to universal seconds, press 54.

+ To calculate the periods in inventory of a nutrition attribute, press 61.
+ To calculate attribute-per-cent (per penny) of a UPC, press 62.
+ To calculate attribute-per-cent (per penny) for a list of UPCs, press 63.
+ To calculate the ratio of attributes-per-cent of two UPCs, press 64.
+ To calculate the attribute-per-cent (per penny) of a recipe, press 65.
+ To calculate the ratio of attributes-per-cent of two recipes, press 66.

+ To see the total value of all assets in inventory, press 81.
+ To see the total value of all assets consumed from inventory, press 82.

+ To see the raw database of stored UPC's and attributes, press 91.
+ To see the raw history of UPC consumption, press 92.
+ To see the raw role-alias associations, press 93.
+ To see the raw recipe list, press 94.
+ To see the raw inventory database, press 95.
+ To see the raw leftovers from recipes, press 96.
+ To see the raw leftovers of individually consumed UPCs, press 97.
+ To see the raw consumption cost log, press 98.

+ To see this menu of options, press 1 or ?.
+ To quit, press 0.~%")

(defparameter +format-string-repl-prompt+
  "~%Use \'?\' for the menu. Remember to press \'Enter\'. ~%> ")
;; start a console I/O session
;; Ctrl+C to (quit) if not wanting to save changes to disk
;; use #'foodstats-loader to start from save files on disk
(defun foodstats-console
    (&optional (format-t t)
       (alist-upc '())
       (alist-history '())
       (alist-recipe '())
       (alist-inventory '())
       (alist-role-alias '())
       (alist-upc-leftover '())
       (alist-recipe-leftover '())
       (alist-consumption-cost '())
       (skip-print-string-main-menu T))
  "Using console I/O, work with the user on their requests.
This is a recursive function.
Each recursive call passes on the current data lists.
Selecting option 0 to quit returns a list of all data structures."
  (progn
    (if
     (not skip-print-string-main-menu)
     (format format-t +format-string-main-menu+))
    (format format-t +format-string-repl-prompt+))
  (let ((main-menu-choice (read-line)))
    (cond
      ;; return the function arguments and exit the console loop
      ((equalp main-menu-choice "0") ;; option 0 to quit
       (list
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ;; print the help menu
      ((or
	(equalp main-menu-choice "?")
	(equalp main-menu-choice "1"))
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost
	;; this final nil parameter tells foodstats-console to print the menu
	nil))
      ;; calculate servings-per-container
      ((equalp main-menu-choice "01") ;; option 01
       (prompt-container-weight-and-serving-size format-t)
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ;; add or update upc attributes in alist-upc
      ((equalp main-menu-choice "02") ;; option 02
       (foodstats-console
	format-t
	(prompt-upc-attributes format-t alist-upc)
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ((equalp main-menu-choice "03") ;; option 03
       (let
	   ((user-upc
	     (prompt-string
	      format-t
	      "Enter the UPC to get information for")))
	 (format
	  format-t
	  "~s~%"
	  (assoc
	   user-upc
	   alist-upc
	   :test #'equalp)))
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ;; add or update an inventory UPC quantity per expiration group
      ((equalp main-menu-choice "04") ;; option 04
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	(prompt-inventory-adjustment
	 format-t
	 alist-upc
	 alist-inventory)
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ((equalp main-menu-choice "05") ;; option 05
       (let
	   ((user-upc
	     (prompt-string
	      format-t
	      "Enter the UPC to check inventory status for")))
	 (format
	  format-t
	  "~s~%"
	  (assoc
	   user-upc
	   alist-inventory
	   :test #'equalp)))
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ;; meat/deli wizard (uses servings-per-container from 1)
      ((equalp main-menu-choice "06") ;; option 06
       (let
	   ((object-name
	     (prompt-string
	      format-t
	      "What is the name of this item?"))
	    (object-weight
	     (prompt-dimensional-unit-pair
	      format-t
	      "How much does it weigh?"))
	    (object-price-per-unit
	     (prompt-string
	      format-t
	      "What is the price per unit?"))
	    (object-total-price
	     (prompt-string
	      format-t
	      "What is the total price of this?"))
	    (object-expiration-date
	     (prompt-string
	      format-t
	      "What is the expiration date?"))
	    (object-serving-size
	     (prompt-dimensional-unit-pair
	      format-t
	      "What is the serving size?"))
	    (object-upc
	     (let
		 ((user-input
		   (prompt-string
		    format-t
		    "UPC, or 0 if you don't have a UPC")))
	       (if
		(equal "0" user-input)
		(get-random-upc alist-upc)
		user-input)))
	    (object-role-alias
	     (prompt-string
	      format-t
	      "What is a role-alias to use for this item?")))
	 (let
	     ((result-alist-role-alias
	       (adjust-alist-role-alias
		alist-role-alias object-upc object-role-alias))
	      (result-alist-upc ;; upc database with new meat item
	       (acons
		object-upc
		(list
		 (cons
		  'name
		  object-name)
		 (cons
		  'serving-size
		  object-serving-size)
		 (cons
		  'servings-per-container
		  (calculate-servings-per-container-from-container-weight
		   object-serving-size 
		   object-weight)))
		alist-upc))
	      (result-alist-inventory ;; create inventory record 
	       (acons
		object-upc
		(list
		 (cons
		  object-expiration-date
		  (list
		   (cons
		    object-total-price
		    1))))
		alist-inventory)))
	   (foodstats-console
	    format-t
	    result-alist-upc ;; new upc database 
	    alist-history
	    alist-recipe
	    result-alist-inventory ;; new inventory database
	    result-alist-role-alias ;; new role alias association
	    alist-upc-leftover
	    alist-recipe-leftover
	    alist-consumption-cost))))
      ;; hypothetical UPC consumption nutrition facts, no logs/inventory.
      ((equalp main-menu-choice "11") ;; option 11
       (format-nutrition-report
	format-t
	alist-upc
	alist-inventory
	alist-role-alias
	(calculate-recipe-portion-percentages-from-dimensional-units
	 alist-upc
	 (prompt-upc-list-by-dimensional-unit format-t alist-upc))
	(prompt-symbol-list format-t alist-upc)
	(prompt-percentage format-t))
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ;; log consumption of a upc to alist-history and alist-inventory
      ((equalp main-menu-choice "12") ;; option 12
       (let
	   ((user-log-time
	     (get-universal-time))
	    (user-log-upc-list
	     (prompt-upc-list-by-dimensional-unit
	      format-t
	      alist-upc)))
	 (let
	     ((user-log-upc-by-percentage
	       (calculate-recipe-portion-percentages-from-dimensional-units
		alist-upc
		user-log-upc-list)))
	   (let
	       ((result-prompt-consume-upc-from-inventory
		 (multiple-value-list 
		  (prompt-consume-upc-from-inventory-adjust-consumption-cost
		   format-t
		   alist-upc
		   alist-inventory
		   alist-upc-leftover
		   alist-consumption-cost
		   user-log-upc-by-percentage
		   user-log-time))))
	     (let 
		 ((result-alist-history
		   (acons
		    user-log-time
		    user-log-upc-list
		    alist-history))
		  (result-alist-inventory
		   (car result-prompt-consume-upc-from-inventory))
		  (result-alist-upc-leftover
		   (cadr result-prompt-consume-upc-from-inventory))
		  (result-alist-consumption-cost
		   (caddr result-prompt-consume-upc-from-inventory)))
	       (foodstats-console
		format-t
		alist-upc
		result-alist-history 
		alist-recipe
		result-alist-inventory 
		alist-role-alias
		result-alist-upc-leftover 
		alist-recipe-leftover
		result-alist-consumption-cost))))))
      ;; list items on the leftover table
      ((equalp main-menu-choice "13") ;; option 13
       (format
	format-t
	"Here are the loose UPC's open on the table (no recipe): ~%~s~%"
	alist-upc-leftover)
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ;; log consumption of leftover UPC's from the table
      ((equalp main-menu-choice "14") ;; option 14
       (let
	   ((user-log-time
	     (get-universal-time))
	    (user-log-upc-percent-list
	     (if
	      (equalp
	       "percentage"
	       (prompt-percentage-or-dimensional-unit format-t))
	      (prompt-upc-list-by-percentage format-t alist-upc)
	      (calculate-recipe-portion-percentages-from-dimensional-units
	       alist-upc
	       (prompt-upc-list-by-dimensional-unit
		format-t
		alist-upc)))))
	 (let
	     ((result-alist-history
	       (acons
		user-log-time 
		(calculate-recipe-dimensional-units-from-portion-percentages
		 alist-upc
		 user-log-upc-percent-list)
		alist-history))
	      (result-alist-upc-leftover
	       (prompt-consume-upc-from-leftover
		format-t
		alist-upc
		alist-upc-leftover
		user-log-upc-percent-list)))
	   (foodstats-console
	    format-t
	    alist-upc
	    result-alist-history
	    alist-recipe
	    alist-inventory
	    alist-role-alias
	    result-alist-upc-leftover
	    alist-recipe-leftover
	    alist-consumption-cost))))
      ;; add or update role-alias / UPC associations
      ((equalp main-menu-choice "31") ;; option 31
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	(prompt-role-upc-assign
	 format-t
	 alist-upc
	 alist-role-alias)
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ;; add or update a named recipe
      ((equalp main-menu-choice "32") ;; option 32
       (foodstats-console
	format-t
	alist-upc
	alist-history
	(adjust-alist-upc-attribute
	 alist-recipe
	 (prompt-string
	  format-t
	  "Enter the recipe name")
	 (cons
	  (prompt-string
	   format-t
	   "Enter the role-alias or UPC")
	  (prompt-dimensional-unit-pair
	   format-t
	   "Enter the amount to put into the recipe")))
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ;; report Nutrition Facts for a named recipe, no logs or inventory
      ((equalp main-menu-choice "41") ;; option 41
       (format-nutrition-report
	format-t
	alist-upc
	alist-inventory
	alist-role-alias
	(calculate-recipe-portion-percentages-from-dimensional-units
	 alist-upc
	 (convert-recipe-role-alias-keys-to-upc-keys-ignore-inventory
	  alist-upc
	  alist-recipe
	  alist-role-alias
	  (get-recipe-upc-dimensional-unit-list
	   alist-recipe
	   (prompt-string
	    format-t
	    "Name of the recipe to generate Nutrition Facts report for:"))))
	(prompt-symbol-list format-t alist-upc)
	(prompt-percentage format-t))
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ((equalp main-menu-choice "42") ;; option 42
       (let
	   ((user-recipe-name
	     (prompt-string format-t "Enter the recipe name"))
	    (user-percent-recipe-consumed
	     (prompt-percentage format-t))
	    (user-log-time
	     (get-universal-time)))
	 (let
	     ((user-log-upc-list
	       (convert-recipe-role-alias-keys-to-upc-keys
		alist-upc
		alist-recipe
		alist-inventory
		alist-role-alias
		(get-recipe-upc-dimensional-unit-list
		 alist-recipe
		 user-recipe-name))))
	   (let
	       ((user-log-upc-by-percentage
		 (calculate-recipe-portion-percentages-from-dimensional-units
		  alist-upc
		  user-log-upc-list)))
	     (let
		 ((result-prompt-consume-upc-from-inventory
		   (multiple-value-list
		    (prompt-consume-upc-from-inventory-adjust-consumption-cost
		     format-t
		     alist-upc
		     alist-inventory
		     alist-upc-leftover
		     alist-consumption-cost
		     user-log-upc-by-percentage
		     user-log-time))))
	       (let
		   ((result-alist-history
		     (acons
		      user-log-time
		      (calculate-percentage-of-dimensional-units
		       user-log-upc-list
		       user-percent-recipe-consumed)
		      alist-history))
		    (result-alist-inventory
		     (car
		      result-prompt-consume-upc-from-inventory))
		    (result-alist-upc-leftover
		     (cadr
		      result-prompt-consume-upc-from-inventory))
		    (result-alist-recipe-leftover
		     (if
		      (> (- 1.0 user-percent-recipe-consumed) 0)
		      (acons
		       user-recipe-name
		       (acons
			user-log-time
			(cons
			 (- 1.0 user-percent-recipe-consumed)
			 user-log-upc-list)
			(cdr
			 (assoc
			  user-recipe-name
			  alist-recipe-leftover
			  :test #'equalp)))
		       (remove
			user-recipe-name
			alist-recipe-leftover
			:key #'car :test #'equalp))
		      alist-recipe-leftover))
		    (result-alist-consumption-cost
		     (caddr result-prompt-consume-upc-from-inventory)))
		 (format
		  format-t "Simulation result (no values written):~%")
		 (format
		  format-t "History:~%~s" result-alist-history)
		 (format
		  format-t "Inventory:~%~s" result-alist-inventory)
		 (format
		  format-t "UPC leftovers:~%~s" result-alist-upc-leftover)
		 (format
		  format-t "Recipe leftovers:~%~s" result-alist-recipe-leftover)
		 (format
		  format-t "Cost log:~%~s" result-alist-consumption-cost))))))
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ;; log consumption of a recipe and update inventory
      ((equalp main-menu-choice "43") ;; option 43
       (let
	   ((user-recipe-name
	     (prompt-string format-t "Enter the recipe name"))
	    (user-percent-recipe-consumed
	     (prompt-percentage format-t)))
	 (let
	     ((user-log-time
	       (get-universal-time))
	      (user-log-upc-list
	       (convert-recipe-role-alias-keys-to-upc-keys
		alist-upc
		alist-recipe
		alist-inventory
		alist-role-alias
		(get-recipe-upc-dimensional-unit-list
		 alist-recipe
		 user-recipe-name))))
	   (let
	       ((user-log-upc-by-percentage
		 (calculate-recipe-portion-percentages-from-dimensional-units
		  alist-upc
		  user-log-upc-list)))
	     (let
		 ((result-prompt-consume-upc-from-inventory
		   (multiple-value-list
		    (prompt-consume-upc-from-inventory-adjust-consumption-cost
		     format-t
		     alist-upc
		     alist-inventory
		     alist-upc-leftover
		     alist-consumption-cost
		     user-log-upc-by-percentage
		     user-log-time))))
	       ;; if the cadddr of that list is nil, 
	       (if ;; then cooking the recipe would create an overage!
		(not
		 (cadddr
		  result-prompt-consume-upc-from-inventory))
		;; no overage, execute the recipe
		(let
		    ((result-alist-history
		      (acons
		       user-log-time
		       (calculate-percentage-of-dimensional-units
			user-log-upc-list
			user-percent-recipe-consumed)
		       alist-history))
		     (result-alist-inventory
		      (car
		       result-prompt-consume-upc-from-inventory))
		     (result-alist-upc-leftover
		      (cadr
		       result-prompt-consume-upc-from-inventory))
		     (result-alist-recipe-leftover
		      (if
		       (> (- 1.0 user-percent-recipe-consumed) 0)
		       (acons
			user-recipe-name
			(acons
			 user-log-time
			 (cons
			  (- 1.0 user-percent-recipe-consumed)
			  user-log-upc-list)
			 (cdr
			  (assoc
			   user-recipe-name
			   alist-recipe-leftover
			   :test #'equalp)))
			(remove
			 user-recipe-name
			 alist-recipe-leftover
			 :key #'car :test #'equalp))
		       alist-recipe-leftover))
		     (result-alist-consumption-cost
		      (caddr result-prompt-consume-upc-from-inventory)))
		  (foodstats-console
		   format-t
		   alist-upc
		   result-alist-history
		   alist-recipe
		   result-alist-inventory
		   alist-role-alias
		   result-alist-upc-leftover
		   result-alist-recipe-leftover
		   result-alist-consumption-cost)
		  ;; not enough goods are available to process the recipe
		  (progn ;; the information as to why the cook failed is 
		    (format ;; in result-prompt-consume-upc-from-inventory
		     format-t 
		     "Not enough inventory to cook the recipe!~%")
		    (foodstats-console
		     format-t
		     alist-upc
		     alist-history
		     alist-recipe
		     alist-inventory
		     alist-role-alias
		     alist-upc-leftover
		     alist-recipe-leftover
		     alist-consumption-cost)))))))))
      ;; simulation consumption of a recipe using leftovers first.
      ((equalp main-menu-choice "44") ;; option 44
       (let
	   ((user-recipe-name
	     (prompt-string format-t "Enter the recipe name"))
	    (user-percent-recipe-consumed
	     (prompt-percentage format-t))
	    (user-log-time
	     (get-universal-time)))
	 (let
	     ((result-calculate-cook-recipe-using-upc-leftovers
	       (calculate-cook-recipe-using-upc-leftovers
		format-t
		alist-upc
		alist-history
		alist-recipe
		alist-inventory
		alist-role-alias
		alist-upc-leftover
		alist-recipe-leftover
		alist-consumption-cost
		user-recipe-name
		user-percent-recipe-consumed
		user-log-time)))
	   (format format-t "Simulation result (nothing was written):~%")
	   (format format-t "~s~%" result-calculate-cook-recipe-using-upc-leftovers)))
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ;; log consumption of a recipe, but use leftovers first.
      ((equalp main-menu-choice "45") ;; option 45
       (let
	   ((user-recipe-name
	     (prompt-string format-t "Enter the recipe name"))
	    (user-percent-recipe-consumed
	     (prompt-percentage format-t))
	    (user-log-time
	     (get-universal-time)))
	 (let
	     ((result-calculate-cook-recipe-using-upc-leftovers
	       (calculate-cook-recipe-using-upc-leftovers
		format-t
		alist-upc
		alist-history
		alist-recipe
		alist-inventory
		alist-role-alias
		alist-upc-leftover
		alist-recipe-leftover
		alist-consumption-cost
		user-recipe-name
		user-percent-recipe-consumed
		user-log-time)))
	   (format
	    format-t "~%~s~%" result-calculate-cook-recipe-using-upc-leftovers)
	   (foodstats-console
	    format-t
	    alist-upc
	    (car result-calculate-cook-recipe-using-upc-leftovers)
	    alist-recipe
	    (cadr result-calculate-cook-recipe-using-upc-leftovers)
	    alist-role-alias
	    (caddr result-calculate-cook-recipe-using-upc-leftovers)
	    (cadddr result-calculate-cook-recipe-using-upc-leftovers)
	    (caddddr result-calculate-cook-recipe-using-upc-leftovers)))))
      ;; view leftover recipes from incomplete consumption
      ((equalp main-menu-choice "46") ;; option 46
       (format
	format-t
	"Here are the recipe leftovers open on the table: ~%~s~%"
	alist-recipe-leftover)
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ;; log consumption of leftover recipe's from the table
      ((equalp main-menu-choice "47") ;; option 47
       (let
	   ((user-recipe-name
	     (prompt-string
	      format-t
	      "Enter the name of the leftover recipe"))
	    (user-recipe-cook-stamp
	     (prompt-integer
	      format-t
	      "Enter the time stamp when the recipe was made"))
	    (user-prompt-percentage
	     (prompt-percentage
	      format-t
	      "What percent will you leave behind? No decimals please: ")))
	 (let 
	     ((user-log-time
	       (get-universal-time))
	      (user-log-upc-percent-list	       
	       (calculate-recipe-portion-percentages-from-dimensional-units
		alist-upc
		(calculate-percentage-of-dimensional-units 
		 (cddr
		  (assoc
		   user-recipe-cook-stamp
		   (cdr
		    (assoc
		     user-recipe-name
		     alist-recipe-leftover
		     :test #'equalp))
		   :test #'equalp))
		 (-
		  (get-recipe-leftover-remaining-percentage
		   alist-recipe-leftover
		   user-recipe-name
		   user-recipe-cook-stamp)
		  user-prompt-percentage)))))
	   (let
	       ((result-alist-history
		 (acons
		  user-log-time
		  (calculate-recipe-dimensional-units-from-portion-percentages
		   alist-upc
		   user-log-upc-percent-list)
		  alist-history))
		(result-alist-recipe-leftover
		 (adjust-alist-recipe-leftover-percentage
		  alist-recipe-leftover
		  user-recipe-name
		  user-recipe-cook-stamp
		  user-prompt-percentage)))
	     (foodstats-console
	      format-t
	      alist-upc
	      result-alist-history
	      alist-recipe
	      alist-inventory
	      alist-role-alias
	      alist-upc-leftover
	      result-alist-recipe-leftover
	      alist-consumption-cost)))))
      ;; calculate exact recipe cost
      ((equalp main-menu-choice "48") ;; option 48
       (let
	   ((user-recipe-name
	     (prompt-string
	      format-t
	      "Enter the name of the recipe to calculate the exact cost of")))
	 (format
	  format-t
	  "The exact total cost to prepare ~s is ~d.~%"
	  user-recipe-name
	  (calculate-recipe-total-cost
	   alist-upc
	   alist-recipe
	   alist-inventory
	   alist-role-alias
	   user-recipe-name)))
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ;; calculate total procurement cost of a recipe
      ((equalp main-menu-choice "49") ;; option 49
       (let
	   ((user-recipe-name
	     (prompt-string
	      format-t
	      "Enter the name of the recipe to calculate the exact cost of")))
	 (format
	  format-t
	  "The total procurement cost to prepare ~s is ~d.~%"
	  user-recipe-name
	  (calculate-recipe-procurement-cost
	   alist-upc
	   alist-recipe
	   alist-inventory
	   alist-role-alias
	   user-recipe-name)))
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ;; generate Nutrition Facts totals between two points of time in the log
      ((equalp main-menu-choice "51") ;; option 51
       (format-nutrition-report
	format-t
	alist-upc
	alist-inventory
	alist-role-alias
	(calculate-recipe-portion-percentages-from-dimensional-units
	 alist-upc
	 (get-upc-list-between-times
      	  alist-history
      	  (prompt-time-range
	   format-t
	   "Enter the time range to generate Nutrition Facts for:~%")))
	(prompt-symbol-list format-t alist-upc)
	1.0) ;; past-tense, asking for percentage here makes no sense!
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ((equalp main-menu-choice "52") ;; option 52
       (format
	format-t
	"You've consumed $~d of inventory in that time duration.~%"
	(calculate-inventory-total-value
	 (list
	  (cons
	   0
	   (get-upc-list-between-times
	    alist-consumption-cost
	    (prompt-time-range
	     format-t
	     "Enter the time range to generate consumption cost report for:~%")
	    )))))
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ((equalp main-menu-choice "53") ;; option 53
       (let
	   ((user-time-stamp
	     (prompt-integer
	      format-t
	      "What is the time stamp? (number of seconds since 01-JAN-1900)"))
	    (user-time-zone
	     (prompt-integer
	      format-t
	      "What time-zone should this be printed for? (-4 for EDT)")))
	 (format-universal-seconds
	  format-t
	  user-time-stamp
	  user-time-zone))
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-consumption-cost))
      ((equalp main-menu-choice "54") ;; option 54
       (format
	format-t
	"The universal seconds format is ~d"
	(prompt-timestamp
	 format-t
	 "Enter the universal seconds (since Jan 1 1900) to decode: "))
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-consumption-cost))
      ;; how many periods are available of some attribute in inventory
      ((equalp main-menu-choice "61") ;; option 61
       (prompt-inventory-attribute-report
	format-t
	alist-upc
	alist-history
	alist-inventory
	T) ;; the final T will cause a print out to format-t
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ;; calculate attribute-per-cent
      ((equalp main-menu-choice "62") ;; option 62
       (let
	   ((user-upc
	     (prompt-string
	      format-t "Which UPC to calculate attribute-per-cent for?"))
	    (user-attribute
	     (prompt-symbol-or-float
	      format-t "Which attribute to calculate per-cent?")))
	 (let
	     ((user-upc-price
	       (read-from-string
		(get-inventory-last-price-group
		 alist-inventory
		 user-upc
		 (get-inventory-last-date-group
		  alist-inventory
		  user-upc)))))
	   (format
	    format-t
	    "The UPC ~s per cent (per penny) is ~d~%"
	    user-attribute
	    (calculate-attribute-per-cent
	     alist-upc
	     user-upc
	     user-attribute
	     user-upc-price))))
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ((equalp main-menu-choice "63") ;; option 63
       (let
	   ((user-attribute
	     (prompt-symbol-or-float
	      format-t
	      "Which attribute to calculate per-cent?")))
	 (format
	  format-t
	  "The UPC list ~s per cent (per penny) is ~d~%"
	  user-attribute
	  (calculate-upc-list-attribute-percent
	   alist-upc
	   alist-inventory
	   (prompt-upc-list-by-dimensional-unit
	    format-t
	    alist-upc)
	   user-attribute)))
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ;; calculate multiplier between two attribute-per-cents between 2 upc's
      ((equalp main-menu-choice "64") ;; option 64
       (let
	   ((user-upc-a
	     (prompt-string
	      format-t "Enter UPC 'A'"))
	    (user-upc-b
	     (prompt-string
	      format-t "Enter UPC 'B'"))
	    (user-attribute
	     (prompt-symbol-or-float
	      format-t "Which attribute to compare per-cent?")))
	 (let
	     ((result-a
	       (calculate-attribute-per-cent
		alist-upc
		user-upc-a
		user-attribute
		(read-from-string
		 (get-inventory-last-price-group
		  alist-inventory
		  user-upc-a
		  (get-inventory-last-date-group
		   alist-inventory
		   user-upc-a)))))
	      (result-b
	       (calculate-attribute-per-cent
		alist-upc
		user-upc-b
		user-attribute
		(read-from-string
		 (get-inventory-last-price-group
		  alist-inventory
		  user-upc-b
		  (get-inventory-last-date-group
		   alist-inventory
		   user-upc-b))))))
	   (if
	    (> result-a result-b)
	    (format
	     format-t
	     "UPC '~s' has ~d times more ~s density per-cent than UPC '~s'.~%"
	     user-upc-a
	     (/ result-a result-b)
	     user-attribute
	     user-upc-b)
	    (format
	     format-t
	     "UPC '~s' has ~d times more ~s density per-cent than UPC '~s'.~%"
	     user-upc-b
	     (/ result-b result-a)
	     user-attribute
	     user-upc-a))))
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ((equalp main-menu-choice "65") ;; option 65
       (let
	   ((user-recipe
	     (prompt-string format-t "Enter a recipe"))
	    (user-attribute
	     (prompt-symbol-or-float
	      format-t "Which attribute to calculate quantity per-cent?")))
	 (let
	     ((result
	       (calculate-recipe-attribute-per-cent
		alist-upc alist-recipe alist-inventory alist-role-alias
		user-recipe user-attribute)))
	   (format
	    format-t
	    "The recipe ~s per cent (penny) is ~d~%"
	    user-attribute
	    result)))
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))       
      ((equalp main-menu-choice "66") ;; option 66
       (let
	   ((user-recipe-a
	     (prompt-string
	      format-t "Enter recipe 'A'"))
	    (user-recipe-b
	     (prompt-string
	      format-t "Enter recipe 'B'"))
	    (user-attribute
	     (prompt-symbol-or-float
	      format-t "Which attribute to compare per-cent?")))
	 (let
	     ((result-a
	       (calculate-recipe-attribute-per-cent
		alist-upc alist-recipe alist-inventory alist-role-alias
		user-recipe-a user-attribute))
	      (result-b
	       (calculate-recipe-attribute-per-cent
		alist-upc alist-recipe alist-inventory alist-role-alias
		user-recipe-b user-attribute)))
   	   (if
	    (> result-a result-b)
	    (format
	     format-t
	     "Recipe '~s' has ~d times more ~s density per-cent than recipe '~s'.~%"
	     user-recipe-a
	     (/ result-a result-b)
	     user-attribute
	     user-recipe-b)
	    (format
	     format-t
	     "Recipe '~s' has ~d times more ~s density per-cent than recipe '~s'.~%"
	     user-recipe-b
	     (/ result-b result-a)
	     user-attribute
	     user-recipe-a))))
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))       
      ((equalp main-menu-choice "81") ;; option 81
       (format
	format-t
	"Total value of un-opened items in inventory: $~d~%"
	(calculate-inventory-total-value alist-inventory))
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ((equalp main-menu-choice "82") ;; option 82
       (format
	format-t
	"Total value of consumed and leftover assets: $~d~%"
	(calculate-inventory-total-value alist-consumption-cost))
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ((equalp main-menu-choice "91") ;; option 91
       (format
	format-t
	"~%Here is the UPC database:~%~%~s~%"
	alist-upc)
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ((equalp main-menu-choice "92") ;; option 92
       (format
	format-t
	"~%The UPC consumption history:~%~s~%"
	alist-history)
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ((equalp main-menu-choice "93") ;; option 93
       (format
	format-t
	"~%The recipe role-alias associations:~%~s~%"
	alist-role-alias)
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ((equalp main-menu-choice "94") ;; option 94
       (format
	format-t
	"~%The recipes:~%~s~%"
	alist-recipe)
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ((equalp main-menu-choice "95") ;; option 95
       (format
	format-t
	"~%The inventory expiration date group quantities:~%~s~%"
	alist-inventory)
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ((equalp main-menu-choice "96") ;; option 96
       (format
	format-t
	"~%The leftovers from recipes:~%~s~%"
	alist-recipe-leftover)
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ((equalp main-menu-choice "97") ;; option 97
       (format
	format-t
	"~%The leftovers from individually consumed UPCs:~%~s~%"
	alist-upc-leftover)
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ((equalp main-menu-choice "98") ;; option 98
       (format
	format-t
	"~%The consumption cost log:~%~s~%"
	alist-consumption-cost)
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost))
      ((equalp main-menu-choice "99") ;; option 99
       (format
	format-t
	"~%Prepare to crash (divide by 0)~%")
       (/ 1 0))
      (t                              ;; no valid option selected
       (format
	format-t
	"~%Please check the menu options carefully!~%")
       (foodstats-console
	format-t
	alist-upc
	alist-history
	alist-recipe
	alist-inventory
	alist-role-alias
	alist-upc-leftover
	alist-recipe-leftover
	alist-consumption-cost)))))

;; load data from disk, and start the program.
;; this function is optional, one could invoke #'foodstats-console directly,
;; and have no data.
(defun foodstats-loader
    (&optional
       (format-t t)
       (alist-upc-file +alist-upc-file+)
       (alist-history-file +alist-history-file+)
       (alist-recipe-file +alist-recipe-file+)
       (alist-inventory-file +alist-inventory-file+)
       (alist-role-alias-file +alist-role-alias-file+)
       (alist-upc-leftover-file +alist-upc-leftover-file+)
       (alist-recipe-leftover-file +alist-recipe-leftover-file+)
       (alist-consumption-cost-file +alist-consumption-cost-file+))
  "Load the foodstats system from disk, and present a console I/O.
This function has two side effects on +alist-upc-file+ and
+alist-history-file+, it will overwrite their default values with the
user-supplied values."
  (format format-t "Loading...")
  (let
      ((alist-upc
	(sexp-deserialize alist-upc-file))
       (alist-history
	(sexp-deserialize alist-history-file))
       (alist-recipe
	(sexp-deserialize alist-recipe-file))
       (alist-inventory
	(sexp-deserialize alist-inventory-file))
       (alist-role-alias
	(sexp-deserialize alist-role-alias-file))
       (alist-upc-leftover
	(sexp-deserialize alist-upc-leftover-file))
       (alist-recipe-leftover
	(sexp-deserialize alist-recipe-leftover-file))
       (alist-consumption-cost
	(sexp-deserialize alist-consumption-cost-file)))
    ;; print the greeting to the user
    (format
     format-t
     "done.~%~%Welcome to Nutrition Facts, a system for tracking your dietary habits.~%")
    ;; launch the console
    (let
	((session-results
	  (foodstats-console
	   format-t
	   (if
	    (eq (car alist-upc) 'let)
	    (eval alist-upc)
	    alist-upc)
	   (if
	    (eq (car alist-history) 'let)
	    (eval alist-history)
	    alist-history)
	   (if
	    (eq (car alist-recipe) 'let)
	    (eval alist-recipe)
	    alist-recipe)
	   (if
	    (eq (car alist-inventory) 'let)
	    (eval alist-inventory)
	    alist-inventory)
	   (if
	    (eq (car alist-role-alias) 'let)
	    (eval alist-role-alias)
	    alist-role-alias)
	   (if
	    (eq (car alist-upc-leftover) 'let)
	    (eval alist-upc-leftover)
	    alist-upc-leftover)
	   (if
	    (eq (car alist-recipe-leftover) 'let)
	    (eval alist-recipe-leftover)
	    alist-recipe-leftover)
	   (if
	    (eq (car alist-consumption-cost) 'let)
	    (eval alist-consumption-cost)
	    alist-consumption-cost)
	   nil ;; this will print the main menu once
	   )))
      ;; after the console returns, save the session-results
      (format format-t "Saving...")
      (sexp-serialize
       (nth 1 session-results)
       alist-upc-file)
      (sexp-serialize
       (nth 2 session-results)
       alist-history-file)
      (sexp-serialize
       (nth 3 session-results)
       alist-recipe-file)
      (sexp-serialize
       (nth 4 session-results)
       alist-inventory-file)
      (sexp-serialize
       (nth 5 session-results)
       alist-role-alias-file)
      (sexp-serialize
       (nth 6 session-results)
       alist-upc-leftover-file)
      (sexp-serialize
       (nth 7 session-results)
       alist-recipe-leftover-file)
      (sexp-serialize
       (nth 8 session-results)
       alist-consumption-cost-file)
      (format format-t "done.~%")))
  (quit))

;; -----------------------------------------------------------------------------
;; the following definitions pertain to test driving the code.
;; this should be the pentultimate section, before the execution section.

;; manually run calculate-nutrition-report-from-recipe-by-portion-percentages
;; which is a "lower" function than the one which accepts dimensional-units in
;; a list. the dimensional-units are converted to percentages of that UPC.
(defun test-1 ()
  "Show that the tree generated by calculate is structured correctly,
after it sums up the requested foodstats (attributes) from each UPC.
Returns an association list."
  (calculate-nutrition-report-from-recipe-by-portion-percentages
   *alist-upc*
   '(("052100107790" . 1.0)
     ("204345808899" . 1.0)
     ("688267020087" . 1.0)
     ("688267027925" . 1.0))
   '(protein sodium) 1.0))

;; test S-expression file I/O
(defun test-2 ()
  "Show that the S-expressions I need are properly seralized/de-serialized. 
Returns T when complete."
  (let ((file "sexp-disk-test.lisp")
	(test-alist-upc '()))
    (progn
      (sexp-serialize *alist-upc* file)
      (setf test-alist-upc (sexp-deserialize file))
      (if (not (equal *alist-upc* test-alist-upc))
	  (format
	   t
	   "*** Either sexp-serialize or sexp-deserialize failed. *** ")
	  T))))

;; -----------------------------------------------------------------------------
;; the following should contain no definitions, but only execution statements.
;; this should be the final section in this file.

;; execute the test(s) upon start-up
;;(test-1)
;;(test-2)
(foodstats-loader)

;; -----------------------------------------------------------------------------
;; EOF foodstats.lisp
