#include <stdio.h>
typedef long long ll;

#define rep(i, n) for(int i = 0; i < (n); ++i)

#define K 30

int N,Q;
int SEND_TO[200005];
int up[K+1][200005]; // up[k][i]: バケツiの所有者を2^k回たどった先
ll sum[K+1][200005]; // sum[k][i]: バケツiに2^k回操作を行ったときに加算される水の総量

int main() {
	scanf("%d%d",&N,&Q);
	rep(i, N) {
		int x;
		scanf("%d", &x);
		x--; // 0-indexed
		SEND_TO[i] = x;
	}
	rep(i,N) {
		up[0][i] = SEND_TO[i];
		sum[0][i] = i+1;
	}
	rep(k, K) {
		rep(i, N) {
			up[k+1][i] = up[k][up[k][i]];
			sum[k+1][i] = sum[k][i] + sum[k][up[k][i]];
		}
	}

	rep(_, Q) {
		int t,b;
		scanf("%d%d",&t,&b);
		b--; // 0-indexed
		ll ans = 0;
		rep(k, K) {
			if((t>>k)&1) {
				ans += sum[k][b];
				b = up[k][b];
			}
		}
		printf("%lld\n", ans);
	}

	return 0;
}
