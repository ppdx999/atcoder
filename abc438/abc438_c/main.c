#include <stdio.h>
int N;
int A[200005];

int main() {
	scanf("%d", &N);
	int top=0;
	for (int i = 0; i < N; i++) {
		int a;
		scanf("%d", &a);
		if (top >= 3 && A[top-1] == a && A[top-2] == a && A[top-3] == a) {
			top -= 3;
		} else {
			A[top++] = a;
		}
	}
	printf("%d\n", top);
	return 0;
}
