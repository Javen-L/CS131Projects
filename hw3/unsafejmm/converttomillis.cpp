#include <iostream>
#include <string>
using namespace std;

int main(int argc, const char* argv[]){
  char cur_char;
  string second_string;
  string other_string;
  string milli_string;
  bool first = false;
  bool m = false;
  bool period = false;
  int acc = 0;
  int count = 0;
  cin >> cur_char;
  while (!cin.eof()){
    if(!first){
      if(cur_char >= '0' || cur_char <= '9'){
	second_string.push_back(cur_char);
	first = true;
      } else {
	cout << cur_char;
      }
    } else {
      if(!m){
	if(cur_char >= '0' && cur_char <= '9'){
	  second_string.push_back(cur_char);
	} else if(cur_char == 'm'){
	  acc = atoi(second_string.c_str())*60000;
	  m = true;
	} else {
	  first = false;
	  cout << second_string;
	  cout << cur_char;
	  second_string.clear();
	}
      } else {
	if(!period){
	  if(cur_char >= '0' && cur_char <= '9'){
	    other_string.push_back(cur_char);
	  } else if(cur_char == '.'){
	    acc += atoi(other_string.c_str())*1000;
	    period = true;
	  } else {
	    first = false;
	    m = false;
	    cout << second_string;
	    cout << 'm';
	    cout << other_string;
	    cout << cur_char;
	    second_string.clear();
	    other_string.clear();
	  }
	} else {
	  if(cur_char >= '0' && cur_char <= '9'){
	    milli_string.push_back(cur_char);
	  } else {
	    first = false;
	    m = false;
	    period = false;
	    acc += atoi(milli_string.c_str());
	    cout << acc;
	    count++;
	    if(count == 3){
	      cout << endl;
	      count = 0;
	    } else {
	      cout << ',';
	    }
	    second_string.clear();
	    other_string.clear();
	    milli_string.clear();
	  }
	}
      }
    }
    cin >> cur_char;
  }
  if(!first){
    if(cur_char >= '0' || cur_char <= '9'){
	second_string.push_back(cur_char);
	first = true;
    } else {
      cout << cur_char;
    }
  } else {
    if(!m){
      if(cur_char >= '0' && cur_char <= '9'){
	second_string.push_back(cur_char);
      } else if(cur_char == 'm'){
	acc = atoi(second_string.c_str())*60000;
	m = true;
      } else {
	first = false;
	cout << second_string;
	cout << cur_char;
	second_string.clear();
      }
    } else {
      if(!period){
	if(cur_char >= '0' && cur_char <= '9'){
	  other_string.push_back(cur_char);
	} else if(cur_char == '.'){
	  acc += atoi(other_string.c_str())*1000;
	  period = true;
	} else {
	  first = false;
	  m = false;
	  cout << second_string;
	  cout << 'm';
	  cout << other_string;
	  cout << cur_char;
	  second_string.clear();
	  other_string.clear();
	}
      } else {
	if(cur_char >= '0' && cur_char <= '9'){
	  milli_string.push_back(cur_char);
	} else {
	  first = false;
	  m = false;
	  period = false;
	  acc += atoi(milli_string.c_str());
	  cout << acc;
	  count++;
	  if(count == 3){
	    cout << endl;
	    count = 0;
	  }
	  second_string.clear();
	  other_string.clear();
	  milli_string.clear();
	}
      }
    }
  }
}
