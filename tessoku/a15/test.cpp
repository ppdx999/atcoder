#include <bits/stdc++.h>
using namespace std;

int remove_duplicates(int* arr, int n){
	int j=0;
	for(int i=1;i<=n;i++)
		if(arr[j]!=arr[i])
            arr[++j]=arr[i];
    return j;
}

void assert_equal(int actual, int expected){
    if(actual!=expected){
        cout << "Assertion Error:" << endl;
        cout << "    actual: " << actual << endl;
        cout << "    expected: " << expected << endl;
        exit(1);
    }
}

int main(void){
    int arr[] = {0, 1, 2, 2, 3, 4, 4, 4, 5, 5};
    int n = 9;

    int expected_arr[] = {0, 1, 2, 3, 4, 5};
    int expected_n = 5;

    int actual_n = remove_duplicates(arr, n);
    assert_equal(actual_n, expected_n);
    for(int i = 0; i < actual_n; i++)
        assert_equal(arr[i], expected_arr[i]);
    return 0;
}