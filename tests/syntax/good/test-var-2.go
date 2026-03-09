package main

import "fmt"

func main() {
    x := 2
    for ; x < 1;  {
        fmt.Print("never")
    }
    fmt.Print("done")
}
