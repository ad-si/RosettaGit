for n in {1..100}; do
  ((( n % 15 == 0 )) && echo 'FizzBuzz') ||
  ((( n % 5 == 0 )) && echo 'Buzz') ||
  ((( n % 3 == 0 )) && echo 'Fizz') ||
  echo $n;
done
