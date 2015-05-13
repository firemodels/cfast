#!/bin/bash
dir=$1
file=$2

if [ ! -e $dir ] ; then
  exit
fi

source ../scripts/get_revision.sh $dir

fullfile=$dir/$file
fullfilebak=$dir/${file}bak
cp $fullfile $fullfilebak

cat << EOF > $fullfile
subroutine get_info (revision,revision_date,compile_date)
implicit none
character(len=256), intent(out) :: revision, revision_date, compile_date
revision = trim(adjustl("$svn_revision"))
revision_date = trim(adjustl("$svn_date"))
compile_date = trim(adjustl("$datetime"))
return
end subroutine get_info
EOF
