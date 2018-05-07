#include <stdio.h>
#include <inttypes.h>


int main() {

  int8_t test = 0;
 uint8_t utest = 0;
 /* printf("%d\n", (int8_t)0b00000001); */
 /* printf("%d\n", (int8_t)0b00000000); */
 /* printf("%d\n", (int8_t)0b11111111); */
 /* printf("%d\n", (int8_t)0b11111110); */

 for (int i; i < 500; i++) {
   test -= 1;
   utest -= 1;
   printf("i: %d\n", test);
   printf("u: %u\n", utest);
 }
return 0;
}
