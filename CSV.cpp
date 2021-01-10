#include <vector>
#include <map>
#include <cstdlib>
#include <string>
#include <iostream>
#include <fstream>
#include <iomanip>
using namespace std;

#define INT typeid(int).name()
#define FLOAT typeid(float).name()
#define DOUBLE typeid(double).name()
#define STRING typeid(string).name()

struct Element{
public:
    bool isNum(){
        if(t == "int") return true;
        for(int i = 0; i < v.size(); i++)
            if(v[i] < '0' || v[i] > '9')
                return false;
        return true;
    }

    void setVal(string s){
        v = s;
        if(isNum()) t = "int";
        else t = t == "null" ? "null" : "string";
    }

    void setVal(int i){
        t = "int";
        v = to_string(i);
    }

    string type(){
        return t;
    }

    string val(){
        if(t == "string") return string("\"" + v + "\"");
        return v;
    }

    int iVal(){
        try{
            if(t != "int" && t != "null")
                throw v + string("cannot convert to int!");
        } catch(string errmsg){
            cout << errmsg << endl;
        }
        if(t == "null") return 0;
        return atoi(v.c_str());
    }

    Element(string s = "null") : v(s){
        if(isNum()) t = "int";
        else t = v == "null" ? "null" : "string";
    }

    Element(int i) : v(to_string(i)){
        t = "int";
    }

private:
    string t;
    string v;
};

vector<string> strSplit(string s, string delimiter){
	vector<string> ans;
	if(delimiter.length() > s.length()) return ans;
	int l = 0;
	for(int i = 0; i < s.length(); ){
		if(s.substr(i, delimiter.length()) == delimiter){
			ans.push_back(s.substr(l, i - l));
			i = i + delimiter.length();
			l = i;
		} else i++;
	}
	if(l < s.length() && s.substr(l) != "") ans.push_back(s.substr(l));
	return ans;
}

bool isNum(string v){
    for(int i = 0; i < v.size(); i++)
        if(v[i] < '0' || v[i] > '9')
            return false;
    return true;
}

class Excel{
public:
    Excel();
    void addCol(string cname);
    void addIndex(string iname);
    Element& operator()(string c, string i);
    Element& operator()(int c, int i);
    vector<string> funcParse(string s);
    vector<string> argParse(string s);
    vector<string> posParse(string s);
    string SUM(string arg);
    string IF(string arg);
    string VLOOKUP(string arg);
    void print(int w = 12);
    //Excel& operator=(string s);

    friend vector<string> strSplit(string s, string delimiter);
private:
    map< int, map< int, Element> > table;
    map<string, int> C;
    map<int, string> indexC;
    map<string, int> I;
    map<int, string> indexI;
};


Excel::Excel(){
    table[1][1] = Element();
}

void Excel::addCol(string cname){
    indexC[C.size() + 1] = cname;
    C[cname] = C.size() + 1;
    for(int i = 1; i < I.size(); i++)
        table[C[cname]][i] = Element();
}

void Excel::addIndex(string iname){
    indexI[I.size() + 1] = iname;
    I[iname] = I.size() + 1;
    for(int i = 1; i < C.size(); i++)
        table[i][I[iname]] = Element();
}

Element& Excel::operator()(string col, string index){
    try{
        if(C.find(col) == C.end() || I.find(index) == I.end())
            throw string("index error : col = " + col + " , index = " + index);
    } catch(string errmsg){
        cout << errmsg;
        exit(0);
    }
    return table[C[col]][I[index]];
}

Element& Excel::operator()(int col, int index){
    try{
        if(col > C.size() || col < 1 || index > C.size() || index < 1)
            throw string("index error : col = " + to_string(col) + " , index = " + to_string(index));
    } catch(string errmsg){
        cout << errmsg;
        exit(0);
    }

    return table[col][index];
}

vector<string> Excel::funcParse(string s){
    vector<string> ans;
    if(strSplit(s, "(").size() > 1){
        ans.push_back(s.substr(0, s.find("(")));
        ans.push_back(s.substr(s.find("(") + 1, s.size() - s.find("(") - 2));
    } else {
        ans.push_back(s);
    }
    return ans;
}

vector<string> Excel::argParse(string s){
    vector<string> args;
    string arg;
    bool flag;
    int pcnt;
    //split by ',' omit ' ', parse ()

    for(int i = 0; i < s.size(); i++){
        arg = "";
        flag = false;
        pcnt = 0;
        while(i < s.size()){
            if(s[i] != ' '){
                if((!flag && s[i] == ',') || (flag && pcnt == 0 && s[i] == ',')) break;
                arg += s[i];
                if(s[i] == '('){
                    flag = true;
                    pcnt++;
                } else if(s[i] == ')'){
                    if(pcnt == 0 || flag == false){
                        cout << "\")\" should appear after \"(\"" << endl;
                        break;
                    }
                    pcnt--;
                }
            }

            i++;
        }

        args.push_back(arg);
    }
    return args;
}

string Excel::SUM(string arg){
    vector<string> parseArg, rangeArg, args;

    args = argParse(arg);

    int sum = 0;
    vector<string> pos1, pos2;

    for(int i = 0; i < args.size(); i++){
        parseArg = funcParse(args[i]);

        if(parseArg.size() > 1){
            args[i] = parseArg[1];
            if(parseArg[0] == "SUM")
                args[i] = SUM(parseArg[1]);
            else if(parseArg[0] == "IF")
                args[i] = IF(parseArg[1]);
            else if(parseArg[0] == "VLOOKUP")
                args[i] = VLOOKUP(parseArg[1]);
        } 
        
        rangeArg = strSplit(args[i], ":");

        //case1 1 num
        if(rangeArg.size() == 1){
            if(isNum(rangeArg[0]))
                sum += atoi(rangeArg[0].c_str());
            else{
                pos1 = posParse(rangeArg[0]);
                sum += table[C[pos1[0]]][I[pos1[1]]].iVal();
            }
        }
        
        //case2 multiple num
        if(rangeArg.size() == 2){
            pos1 = posParse(rangeArg[0]);
            pos2 = posParse(rangeArg[1]);
            //cout << "(" << pos1[0] << "," << pos1[1] << ")->(" << pos2[0] << "," << pos2[1] << ")" << endl;
            for(int i = I[pos1[1]]; i <= I[pos2[1]]; i++)
                for(int c = C[pos1[0]]; c <= C[pos2[0]]; c++)
                    sum += table[c][i].iVal();
        }
        

    }

    return to_string(sum);
}

string Excel::IF(string arg){
    return string("s");
}

string Excel::VLOOKUP(string arg){
    return string("s");
}


vector<string> Excel::posParse(string s){
    vector<string> res;
    string arg1 = "", arg2 = "";
    for(int i = 0; i < s.size(); i++){
        if(s[i] <= 'Z' && s[i] >= 'A')
            arg1 += s[i];
        else if(s[i] <= 'z' && s[i] >= 'a')
            arg2 += s[i];
        else 
            cout << "error character " << s[i] << endl;
    }
    res.push_back(arg1);
    res.push_back(arg2);
    return res;
}

void Excel::print(int w){
    for(int i = 0; i < C.size() + 1; i++)
        for(int j = 0; j < w + 1; j++)
            cout << "-";
    cout << "-" << endl << "|" << setw(w) << "";

    for(int c = 1; c <= indexC.size(); c++)
        cout << "|" << left << setw(w) << indexC[c];
    cout << "|" << endl;

    for(int i = 0; i < C.size() + 1; i++)
        for(int j = 0; j < w + 1; j++)
            cout << "-";
    cout << "-" << endl;

    for(int i = 1; i <= indexI.size(); i++){
        cout << "|" << left << setw(w) << indexI[i];
        for(int c = 1; c <= indexC.size(); c++)
            cout << "|" << left << setw(w) << table[c][i].val();
        cout << "|" << endl;

        for(int i = 0; i < C.size() + 1; i++)
            for(int j = 0; j < w + 1; j++)
                cout << "-";
         cout << "-" << endl;

    }
    cout << endl;
}


int main(int argc, char * argv[]){

    ifstream fin;
    fin.open(argv[1], ios::in);
    vector< vector<string> > csvInput;

    string buf;
    while(getline(fin, buf)){
        csvInput.push_back( strSplit(buf, ",") );
    }
    
    for(int i = 0; i < csvInput.size(); i++){
        for(int c = 0; c < csvInput[0].size(); c++)
            cout << left << setw(12) << csvInput[i][c] << " ";
        cout << endl;
    }

    Excel e;

    if(csvInput.size() > 0){
        for(int c = 1; c < csvInput[0].size(); c++)
            e.addCol(csvInput[0][c]);
        for(int i = 1; i < csvInput.size(); i++)
            e.addIndex(csvInput[i][0]);
        for(int c = 1; c < csvInput[0].size(); c++)
            for(int i = 1; i < csvInput.size(); i++)
                e(c, i) = csvInput[i][c];
    }

    e.print();
    vector<string> args;
    string arg;
    do{
        cout << ">";
        getline(cin, arg);
        args = e.funcParse(arg);
        if(args.size() == 2){
            if(args[0] == "SUM")
                cout << "SUM(" << args[1] << ") = " << e.SUM(args[1]) << endl;
            else if(args[0] == "IF")
                cout << "IF(" << args[1] << ") = " << e.IF(args[1]) << endl;
            else if(args[0] == "VLOOKUP")
                cout << "VLOOKUP(" << args[1] << ") = " << e.VLOOKUP(args[1]) << endl;
        }
    } while(arg != "exit");
    
    return 0;
}