#

# converts all POSIX datetimes to UTC-0800
# indicates status using timezone GMT
UTC8<-function (d)
{
  gmt<-tz(d)=='GMT'
  d<-with_tz(d,'UTC')
  if(any(!gmt))
    d[!gmt]<-d[!gmt]-hours(8)
  d<-force_tz(d,'GMT')
  return (d)
}
