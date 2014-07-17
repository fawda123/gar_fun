#get input-hidden weights and hidden-output weights, remove bias

inp.hid <- best.wts[grep('hidden', names(best.wts))]
split_vals <- substr(names(inp.hid), 1, 8)
inp.hid <- split(inp.hid, split_vals)
inp.hid <- lapply(inp.hid, function(x) t(do.call('rbind', x))[-1, ])

hid.out<-best.wts[[grep(paste('out',out.ind),names(best.wts))]][-1]

# do backwards multiplications for each hidden layer until you get to last
rowSums(apply(inp.hid[[3]], 1, function(x) x * hid.out))
