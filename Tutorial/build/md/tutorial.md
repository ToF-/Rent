First, let's write a test.

## 1. Step 1: setting up the test harness

```diff
 // tests.txt
+Test file for rent
+lines starting with < will go in the input test file
+lines starting with > will go in the expected result file
+
+number of cases
+< 1
+number of orders in the case
+< 1
+< 0 5 1000
+expected result
+> 1000
```

Our Makefile will include a command to launch the test. This includes 4 actions:

- extract the input part of the `tests.txt` file into a file named `input.dat`
- extrxct the output part of the `tests.txt` file into a file named `expected.dat`
- execute our rent program on the input file, towards a `result.dat` file
- compare `expected.dat` and (actual) `result.dat`

## 2. Step 1

```diff
 // Makefile
+rent: rent.c
+	cc rent.c -o rent
+
+test: rent tests.txt
+	sed -n -e 's/^\(< \)\(.*\)/\2/pw input.dat'      tests.txt >/dev/null
+	sed -n -e 's/^\(> \)\(.*\)/\2/pw expected.dat'   tests.txt >/dev/null
+	./rent <input.dat >result.dat
+	diff expected.dat result.dat
+
+clean:
+	rm rent; rm *.dat
 
```

To make our test pass, we only need to write the simplest program that outputs a 1000 value.

## 3. Step 1: setting up the test harness

```diff
 // rent.c
+#include <stdio.h>
+
+int main() {
+    printf("1000\n");
+    return 0;
+}
```

Let's add another case with a unique order, of a different value:

## 4. Step 2 : reading one order cases

```diff
 // tests.txt
 Test file for rent
 lines starting with < will go in the input test file
 lines starting with > will go in the expected result file
 
 number of cases
+< 2
 number of orders in the case
 < 1
 < 0 5 1000
 expected result
 > 1000
 
+< 1
+< 0 5 4807
+> 4807
```

## 5. Step 2

```diff
 // rent.c
 #include <stdio.h>
+#define MAXLINE 80
+
+char Line[MAXLINE];
 
 int main() {
+    int max_cases;
+    fgets(Line, MAXLINE, stdin);
+    sscanf(Line, "%d", &max_cases);
+
+    for(int i=0; i<max_cases; i++) {
+        /* read the number of orders and ignore it */
+        fgets(Line, MAXLINE, stdin); 
+
+        /* read the unique order in the case */
+        fgets(Line, MAXLINE, stdin);
+
+        int start_time, duration, value;
+        sscanf(Line, "%d %d %d", &start_time, &duration, &value);
+        printf("%d\n", value);
+    }
     return 0;
 }
```

## 6. Step 3 adding the value of compatible orders

```diff
 // rent.c
 #include <stdio.h>
 #define MAXLINE 80
 
 char Line[MAXLINE];
 
 int main() {
     int max_cases;
     fgets(Line, MAXLINE, stdin);
     sscanf(Line, "%d", &max_cases);
 
     for(int i=0; i<max_cases; i++) {
         /* read the number of orders and ignore it */
         fgets(Line, MAXLINE, stdin); 
+        int max_orders;
+        sscanf(Line, "%d", &max_orders);
 
+        int total = 0;
+        for(int j=0; j<max_orders; j++) {
             int start_time, duration, value;
 
+            fgets(Line, MAXLINE, stdin);
             sscanf(Line, "%d %d %d", &start_time, &duration, &value);
+            total += value;
+        }
+        printf("%d\n", total);
     }
     return 0;
 }
```
```diff
 // tests.txt
 Test file for rent
 lines starting with < will go in the input test file
 lines starting with > will go in the expected result file
 
 number of cases
+< 3
 number of orders in the case
 < 1
 < 0 5 1000
 expected result
 > 1000
 
 < 1
 < 0 5 4807
 > 4807
 
+two compatible orders result in sum of value
+< 2
+< 0 5 1000
+< 5 9 800
+> 1800
```


