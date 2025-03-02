#include <bits/stdc++.h>
using namespace std;

int main() {
	int n,q;
	int[10000] as;
	cin >> n >> q;
	for(int i=0;i<n;i++) cin >> as[i];
	for(int i=1;i<n;i++) as[i] = as[i-1] + as[i];
	for(int i=0;i<q;i++) {
		int l, r;
		cin >> l >> r;
		cout << as[r] - as[l] << endl;
	}
}
