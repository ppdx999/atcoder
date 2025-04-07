package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	var r = bufio.NewReader(os.Stdin)
	var w = bufio.NewWriter(os.Stdout)
	defer w.Flush()

	var n int
	fmt.Fscan(r, &n)

	fmt.Fprintln(w, n*n)
}
