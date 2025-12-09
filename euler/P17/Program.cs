using System;
using System.Collections;

namespace ProjectEuler._17
{
    class Program
    {
        static private string[] lit_digits =
        {
            null,
            "one",
            "two",
            "three",
            "four",
            "five",
            "six",
            "seven",
            "eight",
            "nine",
            "ten",
            "eleven",
            "twelve",
            "thirteen",
            "fourteen",
            "fifteen",
            "sixteen",
            "seventeen",
            "eighteen",
            "nineteen",
        };

        static private string[] lit_tens =
        {
            null,
            null,
            "twenty",
            "thirty",
            "forty",
            "fifty",
            "sixty",
            "seventy",
            "eighty",
            "ninety",
        };
        static void Main(string[] args)
        {
            int sumOfLetters = 0;

            for (int i = 1; i <= 1000; i++)
            {
                var number = NtoE(i,"","");
                sumOfLetters += number.Length;
                //Console.WriteLine(NtoE(i));
            }
            Console.WriteLine(sumOfLetters);
            Console.ReadLine();
        }

        static string NtoE(int n, string space = " ", string dash = "-")
        {
            if (n < 0 || n > 1000)
                throw new ArgumentOutOfRangeException();
            if (n == 1000)
                return "one" + space + "thousand";

            var words = new ArrayList();
            int hundreds = (n % 1000) / 100; // floor
            int tens = (n % 100) / 10;
            int ones = (n % 10) / 1;
            int mod100 = n % 100;

            if (hundreds >= 1)
            {
                words.Add(lit_digits[hundreds]);
                words.Add("hundred");
                if (mod100 != 0)
                    words.Add("and");
            }
            
            if (mod100 >= 20)
            {
                string lit_mod100 = lit_tens[tens];
                if (ones != 0)
                    lit_mod100 += dash + lit_digits[ones];
                words.Add(lit_mod100);
            }
            else if (mod100 > 0)
                words.Add(lit_digits[mod100]);

            return string.Join(space, words.ToArray());
        }
    }
}
