#include <bits/stdc++.h>
using namespace std;

#define rep1(i, n) for(int i = 0; i < (n); ++i)
#define rep2(i, a, b) for(int i = (a); i < (b); ++i)
#define rep3(i, a, b, s) for(int i = (a); i < (b); i += (s))

#define REP1(i, n) for(int i = 0; i <= (n); ++i)
#define REP2(i, a, b) for(int i = (a); i <= (b); ++i)
#define REP3(i, a, b, s) for(int i = (a); i <= (b); i += (s))

#define drep1(i, n) for(int i = (n) - 1; i >= 0; --i)
#define drep2(i, a, b) for(int i = (b) - 1; i >= (a); --i)
#define drep3(i, a, b, s) for(int i = (b) - 1; i >= (a); i -= (s))

#define DREP1(i, n) for(int i = (n); i >= 0; --i)
#define DREP2(i, a, b) for(int i = (b); i >= (a); --i)
#define DREP3(i, a, b, s) for(int i = (b); i >= (a); i -= (s))

#define GET_MACRO(_1, _2, _3, _4, NAME, ...) NAME
#define rep(...) GET_MACRO(__VA_ARGS__, rep3, rep2, rep1)(__VA_ARGS__)
#define REP(...) GET_MACRO(__VA_ARGS__, REP3, REP2, REP1)(__VA_ARGS__)
#define drep(...) GET_MACRO(__VA_ARGS__, drep3, drep2, drep1)(__VA_ARGS__)
#define DREP(...) GET_MACRO(__VA_ARGS__, DREP3, DREP2, DREP1)(__VA_ARGS__)

#define in(type, ...) type __VA_ARGS__; input(__VA_ARGS__)
void input() {}
template<typename T, typename... Args>
void input(T& a, Args&... args) {
	cin >> a;
	input(args...);
}

#define YES do { cout << "Yes" << endl;} while(0)
#define NO  do { cout << "No" << endl;} while(0)

constexpr int INF = std::numeric_limits<int>::max();
typedef long long ll;

class UF {
	public:
	int par[100009];
	int siz[100009];

	void init(int N) {
		REP(i, 1, N) par[i] = -1;
		REP(i, 1, N) siz[i] = 1;
	}

	int root(int x) {
		while(true) {
			if(par[x] < 0) return x;
			x = par[x];
		}
		return x;
	}

	void unite(int x, int y) {
		int rx = root(x);
		int ry = root(y);
		if(rx == ry) return;
		if(siz[rx] > siz[ry]) {
			par[ry] = rx;
			siz[rx] += siz[ry];
		} else {
			par[rx] = ry;
			siz[ry] += siz[rx];
		}
	}

	bool same(int x, int y) {
		return root(x) == root(y);
	}
};

int main() {
	in(int, N, Q);
	UF uf;
	uf.init(N);

	rep(i, Q){
		in(int, cmd, u, v);
		switch(cmd) {
			case 1: 
				uf.unite(u, v);
				break;
			case 2:
				if(uf.same(u, v)) YES;
				else NO;
				break;
		}
	}
	return 0;
}
