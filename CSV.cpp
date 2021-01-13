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
            if(v[i] != '-' && (v[i] < '0' || v[i] > '9'))
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
        //if(t == "string") return string("\"" + v + "\"");
        return v;
    }

    int iVal(){
        try{
            if(t != "int")
                throw v + string("(type = ") + string(t + ")") + string("cannot convert to int!");
        } catch(string errmsg){
            cout << errmsg << endl;
        }
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

string typeOf(string s){
    if(s == "null" || s == "") return "null";
    if(s[0] == '\"' && s[s.size() - 1] == '\"') return "string";
    return "int"; 
}

bool isNum(string v){
    for(int i = 0; i < v.size(); i++)
        if(v[i] != '-' && v[i] < '0' || v[i] > '9')
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
    vector<string> funcParse(const string s);
    vector<string> argParse(const string s, const string d);
    vector<string> posParse(const string s);
    string valParse(const string s);
    string SUM(string arg);
    string IF(string arg);
    string VLOOKUP(string arg);
    void print(int w = 12);

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
        for(int i = 0; i < col.size(); i++)
            if(col[i] < 'A' || col[i] > 'Z')
                throw string("col name should be capital, you pass : ") + col;
        for(int i = 0; i < index.size(); i++)
            if(index[i] < 'a' || index[i] > 'z')
                throw string("index name should be lower case, you pass : ") + index;
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

vector<string> Excel::argParse(const string s, const string d){
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
                if((!flag && s.substr(i, d.size()) == d) || (flag && pcnt == 0 && s.substr(i, d.size()) == d)){
                    i += (d.size() - 1);
                    break;
                }  
                arg += s[i];
                if(s[i] == '('){
                    flag = true;
                    pcnt++;
                } 
                if(s[i] == ')'){
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

vector<string> Excel::funcParse(const string s){
    string str = "";
    vector<string> ans;

    for(int i = 0; i < s.size(); i++)
        if(s[i] != ' ')
            str += s[i];

    if(strSplit(str, "(").size() > 1){
        ans.push_back(str.substr(0, str.find("(")));
        ans.push_back(str.substr(str.find("(") + 1, str.size() - str.find("(") - 2));
    } else {
        ans.push_back(str);
    }
    return ans;
}

vector<string> Excel::posParse(const string s){
    vector<string> res;
    string arg1 = "", arg2 = "";
    bool capitalOff = false;
    for(int i = 0; i < s.size(); i++){
        try{
            if(s[i] <= 'Z' && s[i] >= 'A'){
                arg1 += s[i];
                capitalOff = true;
            } else if(s[i] <= 'z' && s[i] >= 'a'){
                if(capitalOff)
                    arg2 += s[i];
                else throw string("format error : column name first\n");
            } else throw string(string("error character : (") + s[i] + string(")\n"));
        } catch(const string errmsg){
            cout << errmsg;
            res.push_back("null");
            res.push_back("null");
            return res;
        }
    }
    res.push_back(arg1);
    res.push_back(arg2);
    return res;
}

string Excel::valParse(const string s){
    if(isNum(s)) return s;

    string op, res;
    vector<string> pos, cpArg, funcArg;

    op = "null";
    res = s;

    if(argParse(res, ">=").size() > 1 && funcParse(res).size() > 1)
        op = ">=";
    else if(argParse(res, "<=").size() > 1 && funcParse(res).size() > 1)
        op = "<=";
    else if(argParse(res, "=").size() > 1 && funcParse(res).size() > 1)
        op = "=";
    else if(argParse(res, ">").size() > 1 && funcParse(res).size() > 1)
        op = ">";
    else if(argParse(res, "<").size() > 1 && funcParse(res).size() > 1)
        op = "<";

    //cout << "res1 = " << res << endl;

    if(op != "null"){
        vector<string> lval, rval;
        cpArg = argParse(res, op);

        if(cpArg.size() != 2){
            cout << "size of cpArg in valParse error : " << cpArg.size() << endl;
            return "null"; 
        }

        lval = funcParse(cpArg[0]);
        if(lval.size() > 1){
            if(lval[0] == "SUM")
                cpArg[0] = SUM(lval[1]);
            else if(lval[0] == "IF")
                cpArg[0] = IF(lval[1]);
            else if(lval[0] == "VLOOKUP")
                cpArg[0] = VLOOKUP(lval[1]);
            else {
                cout << "lval error in valParse" << endl;
                res = "null";
            }

            if(!isNum(cpArg[0])){
                pos = posParse(cpArg[0]);
                cpArg[0] = table[C[pos[0]]][I[pos[1]]].val();
            }
        }

        rval = funcParse(cpArg[1]);
        if(rval.size() > 1){
            if(rval[0] == "SUM")
                cpArg[1] = SUM(rval[1]);
            else if(rval[0] == "IF")
                cpArg[1] = IF(rval[1]);
            else if(rval[0] == "VLOOKUP")
                cpArg[1] = VLOOKUP(rval[1]);
            else {
                cout << "rval error in valParse" << endl;
                res = "null";
            }

            if(!isNum(cpArg[1])){
                pos = posParse(cpArg[1]);
                cpArg[1] = table[C[pos[0]]][I[pos[1]]].val();
            }
        }

        if(typeOf(cpArg[0]) != typeOf(cpArg[1])){
            cout << "type inconsistent : lval = " << cpArg[0] << ", rval = " << cpArg[1] << endl;
            return "null";
        }

        if(op == ">"){
            if(atoi(cpArg[0].c_str()) > atoi(cpArg[1].c_str()))
                res = "1";
            else res = "0";
        } else if(op == "<"){
            if(atoi(cpArg[0].c_str()) < atoi(cpArg[1].c_str()))
                res = "1";
            else res = "0";
        } else if(op == "="){
            if(atoi(cpArg[0].c_str()) == atoi(cpArg[1].c_str()))
                res = "1";
            else res = "0";
        } else if(op == ">="){
            if(atoi(cpArg[0].c_str()) >= atoi(cpArg[1].c_str()))
                res = "1";
            else res = "0";
        } else if(op == "<="){
            if(atoi(cpArg[0].c_str()) <= atoi(cpArg[1].c_str()))
                res = "1";
            else res = "0";
        } 

    } else {
        vector<string> val;
        
        val = funcParse(s);
        
        if(val.size() > 1){
            cout << val[0] << " " << val[1] << endl;
            if(val[0] == "SUM")
                res = SUM(val[1]);
            else if(val[0] == "IF")
                res = IF(val[1]);
            else if(val[0] == "VLOOKUP")
                res = VLOOKUP(val[1]);
            else {
                cout << "val error in valParse" << endl;
                res = "null";
            }

            if(!isNum(res)){
                pos = posParse(res);
                res = table[C[pos[0]]][I[pos[1]]].val();
            }
        } else {
            if(strSplit(s, ">=").size() > 1){
                cpArg = strSplit(s, ">=");
                if(!isNum(cpArg[0])){
                    pos = posParse(cpArg[0]);
                    cpArg[0] = table[C[pos[0]]][I[pos[1]]].val();
                }

                if(!isNum(cpArg[1])){
                    pos = posParse(cpArg[1]);
                    cpArg[1] = table[C[pos[0]]][I[pos[1]]].val();
                }

                if(typeOf(cpArg[0]) != typeOf(cpArg[1])){
                    cout << "type inconsistent : lval = " << cpArg[0] << ", rval = " << cpArg[1] << endl;
                    return "null";
                }

                if(atoi(cpArg[0].c_str()) >= atoi(cpArg[1].c_str()))
                    res = "1";
                else res = "0";
            } else if(strSplit(s, "<=").size() > 1){
                cpArg = strSplit(s, "<=");
                if(!isNum(cpArg[0])){
                    pos = posParse(cpArg[0]);
                    cpArg[0] = table[C[pos[0]]][I[pos[1]]].val();
                }

                if(!isNum(cpArg[1])){
                    pos = posParse(cpArg[1]);
                    cpArg[1] = table[C[pos[0]]][I[pos[1]]].val();
                }

                if(typeOf(cpArg[0]) != typeOf(cpArg[1])){
                    cout << "type inconsistent : lval = " << cpArg[0] << ", rval = " << cpArg[1] << endl;
                    return "null";
                }

                if(atoi(cpArg[0].c_str()) <= atoi(cpArg[1].c_str()))
                    res = "1";
                else res = "0";
            } else if(strSplit(s, "=").size() > 1){
                cpArg = strSplit(s, "=");
                if(!isNum(cpArg[0])){
                    pos = posParse(cpArg[0]);
                    cpArg[0] = table[C[pos[0]]][I[pos[1]]].val();
                }

                if(!isNum(cpArg[1])){
                    pos = posParse(cpArg[1]);
                    cpArg[1] = table[C[pos[0]]][I[pos[1]]].val();
                }

                if(typeOf(cpArg[0]) != typeOf(cpArg[1])){
                    cout << "type inconsistent : lval = " << cpArg[0] << ", rval = " << cpArg[1] << endl;
                    return "null";
                }

                if(cpArg[0] == cpArg[1])
                    res = "1";
                else res = "0";

            } else if(strSplit(s, ">").size() > 1){
                cpArg = strSplit(s, ">");
                if(!isNum(cpArg[0])){
                    pos = posParse(cpArg[0]);
                    cpArg[0] = table[C[pos[0]]][I[pos[1]]].val();
                }

                if(!isNum(cpArg[1])){
                    pos = posParse(cpArg[1]);
                    cpArg[1] = table[C[pos[0]]][I[pos[1]]].val();
                }

                if(typeOf(cpArg[0]) != typeOf(cpArg[1])){
                    cout << "type inconsistent : lval = " << cpArg[0] << ", rval = " << cpArg[1] << endl;
                    return "null";
                }

                if(atoi(cpArg[0].c_str()) > atoi(cpArg[1].c_str()))
                    res = "1";
                else res = "0";
            } else if(strSplit(s, "<").size() > 1){
                cpArg = strSplit(s, "<");
                if(!isNum(cpArg[0])){
                    pos = posParse(cpArg[0]);
                    cpArg[0] = table[C[pos[0]]][I[pos[1]]].val();
                }

                if(!isNum(cpArg[1])){
                    pos = posParse(cpArg[1]);
                    cpArg[1] = table[C[pos[0]]][I[pos[1]]].val();
                }

                if(typeOf(cpArg[0]) != typeOf(cpArg[1])){
                    cout << "type inconsistent : lval = " << cpArg[0] << ", rval = " << cpArg[1] << endl;
                    return "null";
                }

                if(atoi(cpArg[0].c_str()) < atoi(cpArg[1].c_str()))
                    res = "1";
                else res = "0";
            }

            if(!isNum(res) && typeOf(res) != "string" && strSplit(res, ":").size() == 1){
                pos = posParse(res);
                res = table[C[pos[0]]][I[pos[1]]].val();
            }
        }
    }
    cout << "res : " << res << endl;
    return res;
}

string Excel::SUM(string arg){
    vector<string> funcArg, rangeArg, args;

    args = argParse(arg, ",");

    int sum = 0;
    vector<string> pos1, pos2;

    for(int i = 0; i < args.size(); i++){
        cout << "sum arg : " << args[i] << endl;
        args[i] = valParse(args[i]);
        if(typeOf(args[i]) != "int"){
            cout << "non int type in sum" << endl;
            return "null";
        } 
    }
    
    for(int i = 0; i < args.size(); i++){
        rangeArg = strSplit(args[i], ":");

        //case1 1 num
        if(rangeArg.size() == 1){
            if(isNum(rangeArg[0]))
                sum += atoi(rangeArg[0].c_str());
            else{
                pos1 = posParse(rangeArg[0]);
                if(table[C[pos1[0]]][I[pos1[1]]].val() == "null")
                    return "null";
                sum += table[C[pos1[0]]][I[pos1[1]]].iVal();
            }
        }
        
        //case2 multiple num
        if(rangeArg.size() == 2){
            //cout << "case 2 : " << rangeArg[0] << " " << rangeArg[1] << endl;
            pos1 = posParse(rangeArg[0]);
            pos2 = posParse(rangeArg[1]);
            ////cout << "(" << pos1[0] << "," << pos1[1] << ")->(" << pos2[0] << "," << pos2[1] << ")" << endl;
            for(int i = I[pos1[1]]; i <= I[pos2[1]]; i++)
                for(int c = C[pos1[0]]; c <= C[pos2[0]]; c++){
                    if(table[c][i].val() == "null")
                        return "null";
                    sum += table[c][i].iVal();
                }
        }
    }

    return to_string(sum);
}

string Excel::IF(string arg){
    vector<string> funcArg, cpArg, args;
    string res, cpOp = "none";

    cout << "if arg = " << arg << endl;

    args = argParse(arg, ",");


    if(args.size() == 3){
        for(int i = 0; i < args.size(); i++){
            cout << "if arg : " << args[i] << endl;
            args[i] = valParse(args[i]);
        }

        if(strSplit(args[0], ":").size() == 1){
            res = args[0] == "1" ? args[1] : (args[0] == "0" ? args[2] : "null");
        } else {
            cout << "the \":\" shouldn't appear in first arg of IF" << endl;
            res = "null";
        }
    } else {
        cout << "num of arg in IF is not correct, expect 3 given, get " << args.size() << endl;
        res = "null";
    }

    return res;
}

string Excel::VLOOKUP(string arg){
    vector<string> args, rangeArg, pos;
    string res;

    args = argParse(arg, ",");

    if(args.size() == 4){
        for(int i = 0; i < 4; i++){
            args[i] = valParse(args[i]);
            cout << "vlookup arg " << i << " = " << args[i] << endl;
        }

        rangeArg = strSplit(args[1], ":");

        if(typeOf(args[0]) != "null" && rangeArg.size() == 2 && typeOf(args[2]) == "int" && (typeOf(args[3]) == "False" || typeOf(args[3]) == "0")){
            vector<string> pos1, pos2;
            int diff = atoi(args[2].c_str()) - 1;
            pos1 = posParse(rangeArg[0]);
            pos2 = posParse(rangeArg[1]);
            for(int i = I[pos1[1]]; i <= I[pos2[1]]; i++)
                for(int c = C[pos1[0]]; c <= C[pos2[0]]; c++)
                    if(table[c][i].val() == args[0]){
                        res = table[C[pos1[0]] + diff][i].val();
                        break;
                    }
        } else {
            cout << "something error in VLOOKUP" << endl;
            for(int i = 0; i < 4; i++)
                cout << "vlookup arg : " << i << " " << args[i] << endl;
            return "null";
        }

    } else {
        cout << "num of arg should be 4 in VLOOKUP, get :" << args.size() << endl;
        return "null";
    }

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
                e(c, i) = e.valParse(csvInput[i][c]);
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
                cout << "SUM(" << args[1] << ") = \n" << e.SUM(args[1]) << endl;
            else if(args[0] == "IF")
                cout << "IF(" << args[1] << ") = \n" << e.IF(args[1]) << endl;
            else if(args[0] == "VLOOKUP")
                cout << "VLOOKUP(" << args[1] << ") = \n" << e.VLOOKUP(args[1]) << endl;
        }
    } while(arg != "exit");
    
    return 0;
}