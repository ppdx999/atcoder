#include <stdio.h>
#define rep(i, n) for (int i = 0; i < (n); i++)

int main() {
	int n,m;
	scanf("%d%d", &n, &m);
	if((n+1)/2 >= m) {
		printf("Yes\n");
	} else {
		printf("No\n");
	}
}
