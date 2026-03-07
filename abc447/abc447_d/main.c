#include <stdio.h>

char S[10000009];
typedef struct {
	int idx;
	int val[10000009];
} Stack;

Stack A,B,C;

void push(Stack *q, int idx) {
	q->val[q->idx++] = idx;
}

int popable(Stack *q) {
	return q->idx > 0;
}

int pop(Stack *q) {
	return q->val[--q->idx];
}

int main() {
	scanf("%s", S);
	int n = 0;
	while (S[n]) n++;

	for (int i = 0; S[i]; i++) {
		if (S[i] == 'A') push(&A, i);
		else if (S[i] == 'B') push(&B, i);
		else push(&C, i);
	}
	int ans = 0;

	while(popable(&A) && popable(&B) && popable(&C)) {
		int a,b,c;
		c = pop(&C);
		while (popable(&B) && (b = pop(&B)) > c);
		while (popable(&A) && (a = pop(&A)) > b);
		if (a < b && b < c) ans++;
	}

	printf("%d\n", ans);
	return 0;
}
