import numpy as np
def updated_weight(cluster_centers_previous,cluster_centers_current,exemplar_weight_previous,labels_current,lambta):
    exemplar_weight_current=[]
    for i in range(len(cluster_centers_current)):
        # this for loop is used to count the number of data items that arrived during the
        #current epoch i and are assigned to the ith cluster 
        count=0
        for m in range(len(labels_current)):
            if labels_current[m]==i:
                count=count+1
        # update the current weight
        for j in range(len(cluster_centers_previous)):
            #when the previous exemplar is the same with the current exemplar
            if cluster_centers_previous[j].all==cluster_centers_current[i].all:
                exemplar_weight=2**(-lambta)*exemplar_weight_previous[j]+count
            #if there is no exemplar is the same with the current exemplar
            else:
                exemplar_weight=count
        exemplar_weight_current.append(exemplar_weight)
    exemplar_weight_current=np.array(exemplar_weight_current)
    return exemplar_weight_current

def updated_weighted_dissimilarity(cluster_centers_previous,cluster_centers_current,weighted_dissimilarity_previous,labels_current,data_current,lambta):
    weighted_dissimilarity_current=[]
    for i in range(len(cluster_centers_current)):
        s=0
        for m in range(len(labels_current)):
            #update the second part in the formulation of weighted dissimilarity 
            if labels_current[m]==i:
              s=s+sum((data_current[m]-cluster_centers_current[i])**2)
        for j in range(len(cluster_centers_previous)):
            if cluster_centers_previous[j].all==cluster_centers_current[i].all:
                weighted_dissimilarity=2**(-lambta)*weighted_dissimilarity_previous[j]+s
            else:
                weighted_dissimilarity=s
        weighted_dissimilarity_current.append(weighted_dissimilarity)
    weighted_dissimilarity_current=np.array(weighted_dissimilarity_current)
    return  weighted_dissimilarity_current