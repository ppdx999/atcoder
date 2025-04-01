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

struct Edge {
  int to, cap, rev;
};

class MaxFlow {
public:
  int size_ = 0;
  bool used[409];
  vector<Edge> G[409];

  void init(int N) {
    size_ = N;
    REP(i,N) G[i].clear();
  }

  void add_edge(int u, int v, int cap) {
    int size_u = G[u].size();
    int size_v = G[v].size();
    // 順辺を追加
    G[u].push_back(Edge{v, cap, size_v});
    // 逆辺を追加
    G[v].push_back(Edge{u, 0, size_u});
  }

  int dfs(int pos, int goal, int minF) {
    if(pos==goal) return minF; // ゴールに到達

    used[pos] = true;

    for(auto& e : G[pos]) {
      // 容量0の辺は使用できない
      if(e.cap == 0) continue;

      // 到達済の頂点に行っても意味がない
      if (used[e.to]) continue;

      // 最小流量を計算
      int flow = dfs(e.to, goal, min(minF, e.cap));

      // グラフを更新
      if (flow >= 1) {
        e.cap -= flow;
        G[e.to][e.rev].cap += flow;
        return flow;
      }
    }

    // 到達不能
    return 0;
  }

  int max_flow(int start, int goal) {
    int sum = 0;
    while(true) {
      REP(i, size_) used[i] = false;
      int F = dfs(start, goal, INF);

      if(F==0) break;
      sum += F;
    }
    return sum;
  }
};

int main() {
  in(int,N,M);
  MaxFlow Z;
  Z.init(N);
  rep(i,M){
    in(int,u,v,cap);
    Z.add_edge(u,v,cap);
  }
  cout << Z.max_flow(1,N) << endl;
	return 0;
}
