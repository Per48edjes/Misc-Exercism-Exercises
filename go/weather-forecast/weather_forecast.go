// Package weather provides a library for obtaining weather forecast information.
package weather

var (
	// CurrentCondition holds the current weather condition.
	CurrentCondition string
	// CurrentLocation holds the current location.
	CurrentLocation string
)

// Forecast returns the weather forecast for the specified city and condition.
func Forecast(city, condition string) string {
	CurrentLocation, CurrentCondition = city, condition
	return CurrentLocation + " - current weather condition: " + CurrentCondition
}
