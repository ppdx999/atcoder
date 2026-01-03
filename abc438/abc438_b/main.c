#include <stdio.h>
#include <limits.h>

static inline int min(int a, int b) {
    return a < b ? a : b;
}

int N, M;
char S[105], T[105];

int diff(int a, int b) {
	return ((a - b) % 10 + 10) % 10;
}

int cost(char* s, char* t, int m) {
	int total = 0;
	for (int i = 0; i < m; i++) total += diff(s[i], t[i]);
	return total;
}

int main() {
	scanf("%d %d", &N, &M);
	scanf("%s", S+1);
	scanf("%s", T+1);
	// convert from char to int
	for (int i = 1; i <= N; i++) S[i] -= '0';
	for (int i = 1; i <= M; i++) T[i] -= '0';

	int ans = INT_MAX;
	for (int i = 1; i <= N - M + 1; i++) {
		ans = min(ans, cost(S + i, T + 1, M));
	}

	printf("%d\n", ans);
}
