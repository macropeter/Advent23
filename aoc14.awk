BEGIN {print "(";}
{print "(";
gsub(/\./, "nil ", $0);
gsub(/\O/, "1   ", $0);
gsub(/\#/, "0   ", $0);
print $0;
print ")";}
END {print ")";
    print NF;
    print NR;}
