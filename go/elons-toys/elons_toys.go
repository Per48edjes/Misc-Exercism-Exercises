package elon

import "fmt"

func (c *Car) Drive() {
	if c.battery >= c.batteryDrain {
		c.battery -= c.batteryDrain
		c.distance += c.speed
	}
}

func (c *Car) DisplayDistance() string {
	return fmt.Sprintf("Driven %v meters", c.distance)
}

func (c *Car) DisplayBattery() string {
	return fmt.Sprintf("Battery at %v%%", c.battery)
}

func (c *Car) CanFinish(trackDistance int) bool {
	batteryRequired := ceilingDivide(trackDistance, c.speed) * c.batteryDrain
	return batteryRequired <= c.battery
}

func ceilingDivide(x, y int) int {
	if y == 0 {
		panic("division by zero")
	}
	return (x + y - 1) / y
}
