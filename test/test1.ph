package test1

type Vec2 object {
  x, y real
}

type VecAdder interface {
  Add(b Vec2) Vec2
}

func main(args []string) int {
  return 0
}

func (self Vec2) Add(b Vec2) Vec2 {
  return Vec2 { 0, 0 }
}
