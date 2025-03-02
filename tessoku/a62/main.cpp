#include <bits/stdc++.h>
#include <vector>
using namespace std;

vector<int> G[100001];
int visited[100001] = {0};

void dfs(int x) {
	if(visited[x]) return;
	visited[x] |= 1;
	for(auto node : G[x]) dfs(node);
}

int main() {
	int n, m; cin >> n >> m;
	for (int i=1; i<=m; ++i) {
		int a, b; cin >> a >> b;
		G[a].push_back(b);
		G[b].push_back(a);
	}

	dfs(1);

	int connected = 1;
	for (int i=1; i<=n; ++i) {
		connected &= visited[i];
	}
	if(connected) cout << "The graph is connected.";
	else cout << "The graph is not connected.";
	cout << endl;

	return 0;
}
