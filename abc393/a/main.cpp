#include <bits/stdc++.h>
using namespace std;

int main() {
  int ans = 4;
	std::string s1, s2;
	cin >> s1;
  cin >> s2;
  if (s1 == "sick") ans -= 2;
  if (s2 == "sick") ans -= 1;
  cout << ans << endl;
}
