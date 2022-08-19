(let ((beef-upc "204345808899")
	    (beef-weight-dimensional-unit '(1.66 . pounds)))
	(list
	 (cons beef-upc
	       (list
		'(name . "USDA 80/20 protein/fat beef")
		'(serving-size . (4 . ounces))
		(cons 'servings-per-container
		      (calculate-servings-per-container-from-container-weight
		       '(4 . ounces)
		       beef-weight-dimensional-unit))
		'(protein . (19 . grams))
		'(fat . (22 . grams))
		'(cholesterol . (0.080 . grams))
		'(sodium . (0.075 . grams))
		'(calories . (280 . calories))
		'(carbohydrates . (0 . grams))))
	 (cons "688267027925"
	       (list
		'(name . "GIANT canned black beans")
		'(serving-size . (130 . grams))
		'(servings-per-container . (3.5 . servings))
		'(protein . (8 . grams))
		'(fat . (1 . grams))
		'(cholesterol . (0 . grams))
		'(sodium . (0.350 . grams))
		'(calories . (120 . calories))
		'(carbohydrates . (21 . grams))))
	 (cons "688267020087"
	       (list
		'(name . "GIANT canned diced tomatoes")
		'(serving-size . (121 . grams))
		'(servings-per-container . (3.5 . servings))
		'(protein . (1 . grams))
		'(fat . (0 . grams))
		'(cholesterol . (0 . grams))
		'(sodium . (0.180 . grams))
		'(calories . (25 . calories))
		'(carbohydrates . (5 . grams))))
	 (cons "052100107790"
	       (list
		'(name . "McCormick TEX-MEX Chili Mix")
		(cons 'serving-size (cons (/ 35.0 4.0) 'grams))
		'(servings-per-container . (4 . servings))
		'(protein . (0 . grams))
		'(fat . (1 . grams))
		'(cholesterol . (0 . grams))
		'(sodium . (0.350 . grams))
		'(calories . (30 . calories))
		'(carbohydrates . (5 . grams))))))
