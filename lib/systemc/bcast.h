#ifndef _bcast_h
#define _bcast_h

#include <systemc.h>

template <class T>
SC_MODULE(Bcast2) { 
  sc_fifo_in<T> i;
  sc_fifo_out<T> o_1;
  sc_fifo_out<T> o_2;

  void main(void);

  SC_HAS_PROCESS(Bcast2);
  
 Bcast2(sc_module_name name_, bool trace_=false) :
  modname(name_), sc_module(name_), trace(trace_)
  {
    SC_THREAD(main);
  }

  ~Bcast2() { }

  private:
    sc_module_name modname;
    bool trace;
}; 

template <class T>
void Bcast2<T>::main(void) {
    T d;
    while(1) { 
          d = i->read();
          o_1->write(d);
          o_2->write(d);
          }
}

template <class T>
SC_MODULE(Bcast3) { 
  sc_fifo_in<T> i;
  sc_fifo_out<T> o_1;
  sc_fifo_out<T> o_2;
  sc_fifo_out<T> o_3;

  void main(void);

  SC_HAS_PROCESS(Bcast3);
  
 Bcast3(sc_module_name name_, bool trace_=false) :
  modname(name_), sc_module(name_), trace(trace_)
  {
    SC_THREAD(main);
  }

  ~Bcast3() { }

  private:
    sc_module_name modname;
    bool trace;
}; 

template <class T>
void Bcast3<T>::main(void) {
    T d;
    while(1) { 
          d = i->read();
          o_1->write(d);
          o_2->write(d);
          o_3->write(d);
          }
}

template <class T>
SC_MODULE(Bcast4) { 
  sc_fifo_in<T> i;
  sc_fifo_out<T> o_1;
  sc_fifo_out<T> o_2;
  sc_fifo_out<T> o_3;
  sc_fifo_out<T> o_4;

  void main(void);

  SC_HAS_PROCESS(Bcast4);
  
 Bcast4(sc_module_name name_, bool trace_=false) :
  modname(name_), sc_module(name_), trace(trace_)
  {
    SC_THREAD(main);
  }

  ~Bcast4() { }

  private:
    sc_module_name modname;
    bool trace;
}; 

template <class T>
void Bcast4<T>::main(void) {
    T d;
    while(1) { 
          d = i->read();
          o_1->write(d);
          o_2->write(d);
          o_3->write(d);
          o_4->write(d);
          }
}

template <class T>
SC_MODULE(Bcast5) { 
  sc_fifo_in<T> i;
  sc_fifo_out<T> o_1;
  sc_fifo_out<T> o_2;
  sc_fifo_out<T> o_3;
  sc_fifo_out<T> o_4;
  sc_fifo_out<T> o_5;

  void main(void);

  SC_HAS_PROCESS(Bcast5);
  
 Bcast5(sc_module_name name_, bool trace_=false) :
  modname(name_), sc_module(name_), trace(trace_)
  {
    SC_THREAD(main);
  }

  ~Bcast5() { }

  private:
    sc_module_name modname;
    bool trace;
}; 

template <class T>
void Bcast5<T>::main(void) {
    T d;
    while(1) { 
          d = i->read();
          o_1->write(d);
          o_2->write(d);
          o_3->write(d);
          o_4->write(d);
          o_5->write(d);
          }
}


template <class T>
SC_MODULE(Bcast6) { 
  sc_fifo_in<T> i;
  sc_fifo_out<T> o_1;
  sc_fifo_out<T> o_2;
  sc_fifo_out<T> o_3;
  sc_fifo_out<T> o_4;
  sc_fifo_out<T> o_5;
  sc_fifo_out<T> o_6;

  void main(void);

  SC_HAS_PROCESS(Bcast6);
  
 Bcast6(sc_module_name name_, bool trace_=false) :
  modname(name_), sc_module(name_), trace(trace_)
  {
    SC_THREAD(main);
  }

  ~Bcast6() { }

  private:
    sc_module_name modname;
    bool trace;
}; 

template <class T>
void Bcast6<T>::main(void) {
    T d;
    while(1) { 
          d = i->read();
          o_1->write(d);
          o_2->write(d);
          o_3->write(d);
          o_4->write(d);
          o_5->write(d);
          o_6->write(d);
          }
}
#endif
