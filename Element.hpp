#include <string>
#include <iostream>

struct Element{
public:
    bool isNum(){
        for(int i = 0; i < v.size(); i++)
            if(v[i] < '0' || v[i] > '9')
                return false;
        return true;
    }

    void setVal(std::string s){
        v = s;
        if(isNum()) t = "int";
        else t = t == "null" ? "null" : "string";
    }

    void setVal(int i){
        t = "int";
        v = std::to_string(i);
    }

    std::string val(){
        if(t == "string") return std::string("\"" + v + "\"");
        return v;
    }

    int iVal(){
        try{
            if(t != "int")
                throw v + std::string("cannot convert to int!");
        } catch(std::string errmsg){
            std::cout << errmsg << std::endl;
        }
        return atoi(v.c_str());
    }

    Element(std::string s = "null") : v(s){
        if(isNum()) t = "int";
        else t = t == "null" ? "null" : "string";
    }

private:
    std::string t;
    std::string v;
};