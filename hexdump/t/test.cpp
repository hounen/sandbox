#include<iostream>
#include<fstream>
using namespace std;

int main(void){
//int i=10;
ifstream ifs("test1.cpp");
string str;
getline(ifs,str);
cout << str;
//while(i-- > 5){
//cout << i;
//}
return 0;
}
