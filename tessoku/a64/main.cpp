#include <bits/stdc++.h>
#include <functional>
#include <queue>
#include <tuple>
#include <utility>
#include <vector>
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

constexpr int INF = std::numeric_limits<int>::max();
typedef long long ll;

vector<pair<int, int>> G[100001];
priority_queue<pair<int, int>, vector<pair<int, int>>, greater<pair<int, int>>> Q;
bool visited[100001];
int cost[100001];

int main() {
	int n, m;
	cin >> n >> m;

	rep(i,m){
		int a,b,c;
		cin >> a >> b >> c;
		G[a].push_back(make_pair(b, c));
		G[b].push_back(make_pair(a, c));
	}
	fill(cost, cost + 100001, INF);
	cost[1] = 0;
	visited[1] = false;
	Q.push(make_pair(0, 1));

	while(!Q.empty()) {
		int curr_node = Q.top().second; Q.pop();

		if (visited[curr_node]) continue;
		else visited[curr_node] = true;

		for(auto edge : G[curr_node]) {
			int node = edge.first;
			int	this_cost = edge.second;
			if(cost[node] > cost[curr_node] + this_cost) {
				cost[node] = cost[curr_node] + this_cost;
				Q.push(make_pair(cost[node], node));
			}
		}
	}
	REP(i, 1, n){
		if(cost[i] == INF) cout << -1 << endl;
		else cout << cost[i] << endl;
	}
	return 0;
}
