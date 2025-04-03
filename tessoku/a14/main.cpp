#include <algorithm>
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

int N,K,A[1009],B[1009],C[1009],D[1009];
int P[1000009],Q[1000009];

int main() {
  cin>>N>>K;
  REP(i,1,N){cin>>A[i];}
  REP(i,1,N){cin>>B[i];}
  REP(i,1,N){cin>>C[i];}
  REP(i,1,N){cin>>D[i];}

  REP(x,1,N)
    REP(y,1,N)
      P[(x-1)*N+y]=A[x]+B[y];
  REP(x,1,N)
    REP(y,1,N)
      Q[(x-1)*N+y]=C[x]+D[y];
  sort(Q+1,Q+(N*N)+1);
  REP(i,1,N*N){
    int pos = lower_bound(Q+1,Q+(N*N)+1,K-P[i])-Q;
    if(pos<=N*N && Q[pos] == K-P[i]){
      YES;return 0;
    }
  }
  NO;
  return 0;
}
