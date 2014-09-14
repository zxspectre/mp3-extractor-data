function [eigenval, sigma, projectedData] = loadmatdata()

#scan all .mat files in a folder, perform PCA on data, display scatter

inputData=[];

maxLabelCount=3;

#first 2 letters of varname define label colors
lastlabel="";
palette=hsv(maxLabelCount + 1);
colors=[];              
lastlabelCnt=0;
currentLabelNo=0;

filelist = readdir (pwd);
for ii = 1:numel(filelist)
  #process only .mat files
  if (regexp (filelist{ii}, "^.*mat$"))
    #extract var name from filename (by contract it's equal to name minus extension)
    [nil1,nil2,nil3,nil4,varname] = regexp(filelist{ii},"^(.*)\\.mat$");
    matstruct = load(filelist{ii});
    varname=char(varname{1});
    #extract variable
    data=matstruct.(varname);

    #process labels, contract is that filelist loop is alphanum ordered (should be)
    if (1-strcmp(lastlabel, substr(varname,1,2)))
        #skip first label change (from null to first)
        if(currentLabelNo>0)
            colors=[colors; ones(lastlabelCnt,3).*palette(currentLabelNo,:)];
            fprintf('\nFound %i samples total for %s\n', lastlabelCnt, lastlabel);
            lastlabelCnt=0;
        endif
        lastlabel=substr(varname,1,2);
        currentLabelNo++;
    endif

    lastlabelCnt+=size(data,2);

    #concat with all data matrix
    inputData=[inputData;data'];
    #helps a bit with memory
    clear data;
    clear matstruct;
    clear nil*;
  endif
endfor

#add colors for last label
colors=[colors; ones(lastlabelCnt,3).*palette(currentLabelNo,:)];
fprintf('\nFound %i samples total for %s\n', lastlabelCnt, lastlabel);


mu=ones(1,size(inputData,1))*inputData;
inputData-=(mu./size(inputData,1));

sigma=inputData'*inputData;
sigma=sigma./size(inputData,1);

fprintf('\nCovariance matrix size is %ix%i\n', size(sigma,1), size(sigma,2));

[U,S,V]=svd(sigma);

projectedData = inputData*U(:,1:2);

for j=1:size(S,1),
    eigenval(j)=S(j,j);
end;
eigenval=eigenval';


scatter(projectedData(:,1),projectedData(:,2),3, colors);
