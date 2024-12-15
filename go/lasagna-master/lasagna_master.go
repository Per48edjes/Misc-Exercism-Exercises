package lasagna

func PreparationTime(layers []string, timePerLayer int) int {
	if timePerLayer == 0 {
		timePerLayer = 2
	}
	return timePerLayer * len(layers)
}

func Quantities(layers []string) (noodles int, sauce float64) {
	noodles, sauce = 0, 0.0
	for _, layer := range layers {
		switch layer {
		case "noodles":
			noodles += 50
		case "sauce":
			sauce += 0.2
		}
	}
	return
}

func AddSecretIngredient(friendsList []string, myList []string) {
	myList[len(myList)-1] = friendsList[len(friendsList)-1]
}

func ScaleRecipe(quantities []float64, desiredPortions int) []float64 {
	multiplier := float64(desiredPortions) / 2.0
	scaledRecipe := make([]float64, len(quantities))
	for i, quantity := range quantities {
		scaledRecipe[i] = quantity * multiplier
	}
	return scaledRecipe
}
