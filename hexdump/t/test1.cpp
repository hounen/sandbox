//#include <sstream>
//#include <iostream>
//#include <fstream>
// 
//using namespace std;
// 
//string file_get_contents(const char *filename)
//{
//    ifstream in(filename);
////    if (in.fail())
////    {
////        cerr << "File not found: " << filename << endl;
////        return "";
////    }
//// 
//    std::stringstream buffer;
//    buffer << in.rdbuf();
////    in.close();
//    return buffer.str();
//}
// 
//int main(int argc, char **argv)
//{
//    string contents = file_get_contents("test.c");
//    cout << contents ;
//    return 0;
//}

#include<iostream>
#include<sstream>
//#include<fstream>
#include<istream>
#include<string>

using namespace std;

int main(void)
{
ifstream ifs("test.c");
string str;
//string str;
str << ifs;//.rdbuf();
//cout << str.str();
//ifs >> str;
int is=str.size();
for(int i=0;i!=is;is++)
{
cout << ifs.get();//.str();
}
return 0;
}
