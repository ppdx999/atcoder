package main

import (
	"bufio"
	"fmt"
	"os"
)

var r = bufio.NewReader(os.Stdin)
var w = bufio.NewWriter(os.Stdout)

func main() {
	defer w.Flush()

	var n, x int
	fmt.Fscan(r, &n)
	fmt.Fscan(r, &x)

	for i := 0; i < n; i++ {
		var a int
		fmt.Fscan(r, &a)
		if a == x {
			fmt.Fprintln(w, "Yes")
			return
		}
	}

	fmt.Fprintln(w, "No")
}
