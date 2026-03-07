#include <stdio.h>
#include <stdlib.h>

char S[300010];
char T[300010];

int main() {
	scanf("%s", S);
	scanf("%s", T);
	int s = 0, t = 0;
	int ans = 0;
	while (S[s] && T[t]) {
		int as = 0, at = 0;
		while (S[s] == 'A' && S[s]) {
			s++;
			as++;
		}
		while (T[t] == 'A' && T[t]) {
			t++;
			at++;
		}
		if (S[s] != T[t]) {
			puts("-1");
			return 0;
		}
		ans += abs(as - at);
		s++;
		t++;
	}
	while (S[s]) {
		if (S[s] != 'A') {
			puts("-1");
			return 0;
		}
		s++;
		ans++;
	}
	while (T[t]) {
		if (T[t] != 'A') {
			puts("-1");
			return 0;
		}
		t++;
		ans++;
	}
	printf("%d\n", ans);
	return 0;
}
