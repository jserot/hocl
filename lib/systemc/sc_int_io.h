#ifndef _sc_int_io
#define _sc_int_io

// There is a bug when reading directy to an sc_[u]int (end of streams are not detected properly)
// These two specializations are a work-around

template <int n>
inline ::std::istream& operator >> ( ::std::istream& is, sc_uint<n>& v) {
  long s;
  is >> s;
  v = s;
  return is;
}

template <int n>
inline ::std::istream& operator >> ( ::std::istream& is, sc_int<n>& v) {
  long s;
  is >> s;
  v = s;
  return is;
}

#endif

