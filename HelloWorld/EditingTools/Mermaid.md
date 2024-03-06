
```mermaid
flowchart LR;
    A --> B
    subgraph one
    B --> C --yes--> D 
    end
    D--> E


    A([user \n input]) 
    B(Angent \n Context)
    C{Use a \n Tool?}
    D(Build the \n Answer) 
    E([Response to the User])
    F(Tools)

flowchart BT;
    C --yes-->B
    
```

![alt text](image.png)

```mermaid
graph LR
emperor((朱八八))-.子.->朱五四-.子.->朱四九-.子.->朱百六

朱五四-.太爷.->emperor2

朱雄英--长子-->朱标--长子-->emperor
emperor2((朱允炆))--次子-->朱标
朱樉--次子-->emperor
朱棡--三子-->emperor
emperor3((朱棣))--四子-->emperor
emperor4((朱高炽))--长子-->emperor3
```