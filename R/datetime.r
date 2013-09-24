#

# converts all POSIX datetimes to UTC-0800
UTC8<-function (d)
{
  gmt<-tz(d)=='GMT'
  d<-with_tz(d,'UTC')
  if(any(!gmt))
    d[!gmt]<-d[!gmt]-hours(8)
  d<-force_tz(d,'UTC')
  return (d)
}
