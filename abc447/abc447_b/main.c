#include <stdio.h>
#define rep(i, n) for (int i = 0; i < (n); i++)

int counts[26];
char S[100];

typedef struct {
	char val[100];
	int idx;
} Answer;

void init(Answer *ans) {
	ans->idx = 0;
}

void push(Answer *ans, char c) {
	ans->val[ans->idx++] = c;
}

Answer ans;

int main() {
	scanf("%s", S);
	init(&ans);
	int N = 0;
	while (S[N]) N++;
	rep(i, N) counts[S[i] - 'a']++;
	int max = 0;
	rep(i, 26) if (counts[i] > max) max = counts[i];

	rep(i,N) {
		if (counts[S[i] - 'a'] == max) continue;
		else push(&ans, S[i]);
	}

	printf("%s\n", ans.val);

	return 0;
}
