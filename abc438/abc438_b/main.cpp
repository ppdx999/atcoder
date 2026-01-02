#include<bits/stdc++.h>
using namespace std;

int diff(char a, char b) {
	return ((a - b) % 10 + 10) % 10;
}

int cost(string_view s, string_view t) {
	int total = 0;
	for (int i = 0; i < s.size(); i++) total += diff(s[i], t[i]);
	return total;
}

int main() {
	int N, M;
	cin >> N >> M;
	string S, T;
	cin >> S >> T;

	int ans = INT_MAX;
	for (int i = 0; i <= N - M; i++) {
		ans = min(ans, cost(string_view(S).substr(i, M), T));
	}
	cout << ans << endl;
}
