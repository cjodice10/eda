#write out log file
write_out_log_file = function(f,fout,append){
  write.table( f,
               file=fout,
               append = append,
               sep='\t',
               row.names=FALSE,
               col.names=TRUE )

  write.table( data.frame(x=c("")),
               file=fout,
               append = append,
               sep='\t',
               row.names=FALSE,
               col.names=FALSE )
}
