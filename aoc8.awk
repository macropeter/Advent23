BEGIN {print "(";}
{print "(";}
{gsub(/[=,()]/, "", $0);} # ersetze alle Sonderzeichen
{print ":" $1;print ":" $2;print ":" $3; } #füge : an jeden String vorne an
{print ")";}
END {print ")";}
