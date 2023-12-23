BEGIN {print "(";}
{print "(";
gsub(/\./, "nil ", $0); #Problem: . steht in regex für alles!
gsub("#", "1   ", $0);
print $0;
print ")";}
END {print ")";
    print NF;
    print NR;}
