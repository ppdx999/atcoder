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
int N;
int A[100009], B[100009];
pair<int,int> dp[100009];

int main() {
	cin>>N;
	REP(i,2,N) cin>>A[i];
	REP(i,3,N) cin>>B[i];
	dp[1] = {0,0};
	dp[2] = {A[2],1};
	REP(i,3,N){
		int a_route = dp[i-1].first + A[i];
		int b_route = dp[i-2].first + B[i];
		dp[i] = (a_route <= b_route)?make_pair(a_route,i-1):make_pair(b_route,i-2);
	};

	vector<int> route;
	route.push_back(N);
	int i=N;
	while(dp[i].second != 0){
		route.push_back(dp[i].second);
		i = dp[i].second;
	}
	reverse(route.begin(),route.end());
	cout<<route.size()<<endl;
	rep(i,route.size()){
		if(i!=0)cout<<" ";
		cout<<route[i];
	}
	cout<<endl;
	
	return 0;
}
