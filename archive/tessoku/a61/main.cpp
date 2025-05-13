#include <algorithm>
#include <bits/stdc++.h>
#include <vector>
using namespace std;
#define rep0(i, n) for (int i = 0; i < (n); ++i)
#define rep1(i, n) for (int i = 1; i <= (n); ++i)
#define all(x) x.begin(), x.end()

int A[100001], B[100001];
vector<int> G[100001];

int main() {
	int n, m;
	cin >> n >> m;
	rep1(i,m) {
		cin >> A[i] >> B[i];
		G[A[i]].push_back(B[i]);
		G[B[i]].push_back(A[i]);
	}
	rep1(i,m) {
		sort(all(G[i]));
	}
	rep1(i, n){
		cout << i << ": {";
		rep0(j, G[i].size()){
			if(j>0) cout << ", ";
			cout << G[i][j];
		}
		cout << "}" << endl;
	}
}
