#include <stdio.h>
#include <stdlib.h>
#define rep(i, n) for (int i = 0; i < (n); i++)

typedef struct {
	int u, v;
	int w;
} Edge;

typedef struct {
	int n;
	int *parent;
	int components;
} UnionFind;

UnionFind *uf_create(int n) {
	UnionFind *uf = (UnionFind *)malloc(sizeof(UnionFind));
	uf->n = n;
	uf->parent = (int *)malloc(n * sizeof(int));
	uf->components = n;
	rep(i, n) uf->parent[i] = -1;
	return uf;
}

void uf_destroy(UnionFind *uf) {
	free(uf->parent);
	free(uf);
}

int uf_find(UnionFind *uf, int x) {
	if (uf->parent[x] < 0) return x;
	return uf->parent[x] = uf_find(uf, uf->parent[x]);
}

void uf_unite(UnionFind *uf, int x, int y) {
	x = uf_find(uf, x);
	y = uf_find(uf, y);
	if (x == y) return;
	if(uf->parent[x] > uf->parent[y]) {
		int temp = x;
		x = y;
		y = temp;
	}
	uf->parent[x] += uf->parent[y];
	uf->parent[y] = x;
	uf->components--;
}

int uf_same(UnionFind *uf, int x, int y) {
	return uf_find(uf, x) == uf_find(uf, y);
}

int N, M;
#define MOD 998244353

int main() {
	scanf("%d%d", &N, &M);
	Edge *edges = (Edge *)malloc(M * sizeof(Edge));
	rep(i, M) {
		scanf("%d%d", &edges[i].u, &edges[i].v);
		edges[i].u--;
		edges[i].v--;
		edges[i].w = i;
	}
	long long *pow2 = (long long *)malloc((M + 1) * sizeof(long long));
	pow2[0] = 1;
	for (int i = 1; i <= M; i++) {
		pow2[i] = (pow2[i - 1] * 2) % MOD;
	}
	UnionFind *uf = uf_create(N);

	long long ans = 0;
	for(int i  = M - 1; i >= 0; i--) {
		int u =  edges[i].u;
		int v = edges[i].v;
		if (!uf_same(uf, u, v)) {
			if(uf->components == 2) {
				ans = (ans + pow2[i+1]) % MOD;
			} else {
				uf_unite(uf, u, v);
			}
		}
	}

	printf("%lld\n", ans);
	uf_destroy(uf);
	return 0;
}
