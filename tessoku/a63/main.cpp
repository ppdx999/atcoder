#include <algorithm>
#include <bits/stdc++.h>
#include <queue>
#include <vector>
using namespace std;

// 半開区間 rep
#define rep1(i, n) for(int i = 0; i < (n); ++i)
#define rep2(i, a, b) for(int i = (a); i < (b); ++i)
#define rep3(i, a, b, s) for(int i = (a); i < (b); i += (s))

// 閉区間 REP
#define REP1(i, n) for(int i = 0; i <= (n); ++i)
#define REP2(i, a, b) for(int i = (a); i <= (b); ++i)
#define REP3(i, a, b, s) for(int i = (a); i <= (b); i += (s))

// 半開区間 drep
#define drep1(i, n) for(int i = (n) - 1; i >= 0; --i)
#define drep2(i, a, b) for(int i = (b) - 1; i >= (a); --i)
#define drep3(i, a, b, s) for(int i = (b) - 1; i >= (a); i -= (s))

// 閉区間 DREP
#define DREP1(i, n) for(int i = (n); i >= 0; --i)
#define DREP2(i, a, b) for(int i = (b); i >= (a); --i)
#define DREP3(i, a, b, s) for(int i = (b); i >= (a); i -= (s))

#define GET_MACRO(_1, _2, _3, _4, NAME, ...) NAME
#define rep(...) GET_MACRO(__VA_ARGS__, rep3, rep2, rep1)(__VA_ARGS__)
#define REP(...) GET_MACRO(__VA_ARGS__, REP3, REP2, REP1)(__VA_ARGS__)
#define drep(...) GET_MACRO(__VA_ARGS__, drep3, drep2, drep1)(__VA_ARGS__)
#define DREP(...) GET_MACRO(__VA_ARGS__, DREP3, DREP2, DREP1)(__VA_ARGS__)

#define pb push_back
#define mp make_pair
#define fst first
#define snd second

constexpr int INF = std::numeric_limits<int>::max();
typedef long long ll;

vector<int> G[100001];
int cost[100001];

int main() {
	int n, m; cin >> n >> m;
	rep(i, m){
		int a, b;
		cin >> a >> b;
		G[a].pb(b);
		G[b].pb(a);
	}
	fill(cost, cost + n + 1, INF);

	queue<int> que;
	que.push(1);
	cost[1] = 0;
	while(!que.empty()) {
		int curr = que.front();
		que.pop();
		vector<int> nexts = G[curr];
		int n = nexts.size();

		rep(i, n) {
			int next = nexts.at(i);
			// if first visit
			if(cost[next] == INF){
				que.push(next);
			}

			if (cost[next] > cost[curr] + 1) {
				cost[next] = cost[curr] + 1;
			}
		}
	}

	REP(i, 1, n) if(cost[i] == INF) cost[i] = -1;

	REP(i, 1, n) cout << cost[i] << endl;
}
