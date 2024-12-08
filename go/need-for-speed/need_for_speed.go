package speed

type Car struct {
	battery      int
	batteryDrain int
	speed        int
	distance     int
}

type Track struct {
	distance int
}

// NewCar creates a new remote controlled car with full battery and given specifications.
func NewCar(speed, batteryDrain int) Car {
	if batteryDrain < 0 || batteryDrain > 100 {
		panic("batteryDrain must be >= 0 and <= 100")
	}
	return Car{
		speed:        speed,
		batteryDrain: batteryDrain,
		battery:      100,
	}
}

// NewTrack creates a new track
func NewTrack(distance int) Track {
	if distance < 0 {
		panic("distance must be >= 0")
	}
	return Track{
		distance: distance,
	}
}

// Drive drives the car one time. If there is not enough battery to drive one more time,
// the car will not move.
func Drive(car Car) Car {
	if car.battery >= car.batteryDrain {
		car.battery -= car.batteryDrain
		car.distance += car.speed
	}
	return car
}

// CanFinish checks if a car is able to finish a certain track.
func CanFinish(car Car, track Track) bool {
	if car.speed == 0 {
		return false
	}
	numDrives := (track.distance + car.speed - 1) / car.speed
	return numDrives*car.batteryDrain <= car.battery
}
