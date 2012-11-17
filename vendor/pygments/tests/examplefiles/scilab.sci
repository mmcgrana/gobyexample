// Scilab ( http://www.scilab.org/ )
// Copyright (C) INRIA - Serge STEER
// 

function I=sub2ind(dims,varargin)
//sub2ind is used to determine the equivalent single index
//corresponding to a given set of subscript values.
 
//I = sub2ind(dims,i1,i2,..) returns the linear index equivalent to the
//row,  column, ... subscripts in the arrays i1,i2,..  for an matrix of
//size dims.
 
//I = sub2ind(dims,Mi) returns the linear index
//equivalent to the n subscripts in the columns of the matrix Mi for a matrix
//of size dims.
 
  d=[1;cumprod(matrix(dims(1:$-1),-1,1))]
  for i=1:size(varargin)
    if varargin(i)==[] then I=[],return,end
  end

  if size(varargin)==1 then //subindices are the columns of the argument
    I=(varargin(1)-1)*d+1
  else //subindices are given as separated arguments
    I=1
    for i=1:size(varargin)
      I=I+(varargin(i)-1)*d(i)
    end
  end
endfunction
