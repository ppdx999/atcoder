#include <stdio.h>

typedef long long ll;

static inline ll max(ll a, ll b) { return a > b ? a : b; }

int N;
ll A[1000001];
ll B[1000001];
ll C[1000001];

ll x_max[1000001];
ll y_max[1000001];

int main() {
	scanf("%d", &N);
	for (int i = 0; i < N; i++) scanf("%lld", &A[i]);
	for (int i = 0; i < N; i++) scanf("%lld", &B[i]);
	for (int i = 0; i < N; i++) scanf("%lld", &C[i]);

	// cumulative sums
	for (int i = 1; i < N; i++) {
		A[i] += A[i - 1];
		B[i] += B[i - 1];
		C[i] += C[i - 1];
	}

	// precompute x_max
	x_max[0] = A[0] - B[0];
	for (int i = 1; i < N; i++) {
		x_max[i] = max(x_max[i - 1], A[i] - B[i]);
	}

	ll ans = 0;
	for (int y = 1; y < N-1; y++) {
		ans = max(ans, x_max[y - 1] + B[y] + C[N - 1] - C[y]);
	}

	printf("%lld\n", ans);
}
