#ifndef _bcast_h
#define _bcast_h

#include <systemc.h>

template <class T>
SC_MODULE(bcast2) { 
  sc_in<T> i;
  sc_out<T> o_1;
  sc_out<T> o_2;

  void main(void);

  SC_HAS_PROCESS(bcast2);
  
 bcast2(sc_module_name name_) :
  modname(name_), sc_module(name_)
  {
    SC_METHOD(main);
    sensitive << i;
  }

  ~bcast2() { }

  private:
    sc_module_name modname;
    T d;
}; 

template <class T>
void bcast2<T>::main(void) {
          d = i->read();
          o_1->write(d);
          o_2->write(d);
}

template <class T>
SC_MODULE(bcast3) { 
  sc_in<T> i;
  sc_out<T> o_1;
  sc_out<T> o_2;
  sc_out<T> o_3;

  void main(void);

  SC_HAS_PROCESS(bcast3);
  
 bcast3(sc_module_name name_) :
  modname(name_), sc_module(name_)
  {
    SC_METHOD(main);
    sensitive << i;
  }

  ~bcast3() { }

  private:
    sc_module_name modname;
    T d;
}; 

template <class T>
void bcast3<T>::main(void) {
          d = i->read();
          o_1->write(d);
          o_2->write(d);
          o_3->write(d);
}

template <class T>
SC_MODULE(bcast4) { 
  sc_in<T> i;
  sc_out<T> o_1;
  sc_out<T> o_2;
  sc_out<T> o_3;
  sc_out<T> o_4;

  void main(void);

  SC_HAS_PROCESS(bcast4);
  
 bcast4(sc_module_name name_) :
  modname(name_), sc_module(name_)
  {
    SC_METHOD(main);
    sensitive << i;
  }

  ~bcast4() { }

  private:
    sc_module_name modname;
    T d;
}; 

template <class T>
void bcast4<T>::main(void) {
          d = i->read();
          o_1->write(d);
          o_2->write(d);
          o_3->write(d);
          o_4->write(d);
}

template <class T>
SC_MODULE(bcast5) { 
  sc_in<T> i;
  sc_out<T> o_1;
  sc_out<T> o_2;
  sc_out<T> o_3;
  sc_out<T> o_4;
  sc_out<T> o_5;

  void main(void);

  SC_HAS_PROCESS(bcast5);
  
 bcast5(sc_module_name name_) :
  modname(name_), sc_module(name_)
  {
    SC_METHOD(main);
    sensitive << i;
  }

  ~bcast5() { }

  private:
    sc_module_name modname;
    T d;
}; 

template <class T>
void bcast5<T>::main(void) {
          d = i->read();
          o_1->write(d);
          o_2->write(d);
          o_3->write(d);
          o_4->write(d);
          o_5->write(d);
}


template <class T>
SC_MODULE(bcast6) { 
  sc_in<T> i;
  sc_out<T> o_1;
  sc_out<T> o_2;
  sc_out<T> o_3;
  sc_out<T> o_4;
  sc_out<T> o_5;
  sc_out<T> o_6;

  void main(void);

  SC_HAS_PROCESS(bcast6);
  
 bcast6(sc_module_name name_) :
  modname(name_), sc_module(name_)
  {
    SC_METHOD(main);
    sensitive << i;
  }

  ~bcast6() { }

  private:
    sc_module_name modname;
    T d;
}; 

template <class T>
void bcast6<T>::main(void) {
          d = i->read();
          o_1->write(d);
          o_2->write(d);
          o_3->write(d);
          o_4->write(d);
          o_5->write(d);
          o_6->write(d);
}
#endif
