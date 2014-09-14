function colortest()

maxLabelCount = 3;
palette=hsv(maxLabelCount + 1);
colors=[ones(1,3).*palette(1,:)];
data=[0,0];

for i=1:maxLabelCount,
    colors=[colors; ones(1,3).*palette(i,:)];
    data=[data;i,i];
end;

data=[data;maxLabelCount+1,maxLabelCount+1];
colors=[colors; ones(1,3).*palette(maxLabelCount + 1,:)];

scatter(data(:,1),data(:,2),20, colors);
