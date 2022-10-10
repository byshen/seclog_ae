int result_check_1(int ret){
  return ret == 0;
}

int main() {
  if (result_check_1(200)){
    return -1;
  }
  return 0;
}
