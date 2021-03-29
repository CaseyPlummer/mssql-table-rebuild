/********************************************************************************************************
Description:    Rebuilds a MSSQL table with a specified column order. Can also drop/add an identity constraint.

                It is HIGHLY recommended to verify table changes with a schema comparison tool to ensure script accuracy.
                Uses table function Split which splits a string into a table, with its position.
                Uses stored procedure PrintMax instead of PRINT so more than 4000 characters can be printed.

                USE THIS AT YOUR OWN RISK. DATA OR SCHEMA LOSS MAY RESULT.

*********************************************************************************************************/
CREATE PROCEDURE [dbo].[TableRebuild]
     @SchemaName SYSNAME = 'dbo'
    ,@TableName SYSNAME
    ,@ColumnNamesAsCsv NVARCHAR(MAX) = NULL     -- The intended column order. All column names must match. Matching is case insensitive. New columns will use specified case. Can be NULL if @DropIdentityConstraint=1 OR @AddIdentityConstraint=1
    ,@DropIdentityConstraint BIT = 0            -- The only way to drop an identity constraint is to rebuild the table.
    ,@AddIdentityConstraint BIT = 0             -- The only way to add an identity constraint is to rebuild the table. Assumes it being added to the primary key.
    ,@AddIdentitySeed INT = 1                   -- The new identity seed to add.
    ,@AddIdentityIncrement INT = 1              -- The new identity increment to add.
    ,@Verbosity INT = 3                         -- 0=Quiet, 1=Errors, 2=Warnings, 3=Info, 4=Debug
    ,@GenerateScriptOnly bit = 0                -- If true, only generates the SQL script but does not execute it.
AS
SET NOCOUNT ON

---- Developer/testing code:
--DECLARE @SchemaName sysname = 'dbo'
--DECLARE @TableName sysname = 'tblReport'
--DECLARE @ColumnNamesAsCsv NVARCHAR(MAX) = NULL
--DECLARE @DropIdentityConstraint BIT = 0
--DECLARE @AddIdentityConstraint BIT = 1
--DECLARE @AddIdentitySeed INT = 1
--DECLARE @AddIdentityIncrement INT = 1
--DECLARE @Verbosity INT = 2
--DECLARE @GenerateScriptOnly BIT = 0

-- Set NULL to defaults
SET @SchemaName=ISNULL(@SchemaName,'dbo')
SET @DropIdentityConstraint=ISNULL(@DropIdentityConstraint,0)
SET @AddIdentityConstraint=ISNULL(@AddIdentityConstraint,0)
SET @AddIdentitySeed=ISNULL(@AddIdentitySeed,1)
SET @AddIdentityIncrement=ISNULL(@AddIdentityIncrement,1)
SET @Verbosity=ISNULL(@Verbosity,3)
SET @GenerateScriptOnly=ISNULL(@GenerateScriptOnly,0)

-- Validate parameters
SET @SchemaName=ISNULL(@SchemaName,'dbo')
IF ISNULL(@TableName,'')='' RAISERROR('Error: The @TableName parameter is required.', 18, 1)
-- @ColumnNamesAsCsv is validated lower

DECLARE
     @PrimaryTable nvarchar(max), @ForeignTable nvarchar(max)
    ,@ErrorMessage varchar(2048), @Message varchar(max)
    ,@PrintStart varchar(100), @PrintEnd varchar(100), @LF char(1)

SET @LF = CHAR(10)
SET @PrimaryTable = '[' + @SchemaName + '].[' + @TableName + ']'
SET @PrintStart=''
SET @PrintEnd=''
IF @GenerateScriptOnly=1
BEGIN
    SET @Verbosity=2
    SET @PrintStart = @LF + 'PRINT '''
    SET @PrintEnd=''''
END

-- Rebuild the table if it exists
IF NOT EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_SCHEMA = @SchemaName AND TABLE_NAME = @TableName)
BEGIN
    SET @ErrorMessage = 'Error: Cannot rebuild the table ' + @PrimaryTable + ' because it does not exist.'
    RAISERROR (@ErrorMessage, 18, 1)
END
ELSE
BEGIN

    ------------------------------
    ----- Gather Information -----
    ------------------------------
    
    -- Gather information about the triggers for this table
    DECLARE @Triggers TABLE (
         TriggerName sysname
        ,TriggerDefinition nvarchar(max)
    )

    INSERT INTO @Triggers
    SELECT
        TriggerName         = tr.name
       ,TriggerDefinition   = OBJECT_DEFINITION(tr.object_id)
    FROM sys.triggers tr
    JOIN sys.tables t
        ON t.object_id = tr.parent_id
    WHERE
        SCHEMA_NAME(t.schema_id) = @SchemaName AND t.name=@TableName

    -- Gather information about the indexes for this table
    DECLARE @TableIndexColumns TABLE (
         IndexId int
        ,IsUnique bit
        ,IsConstraint bit
        ,IndexType sysname
        ,ConstraintOrIndexName sysname
        ,IndexColumnId int
        ,IsIncludedColumn bit
        ,KeyOrdinal int
        ,ColumName sysname
        ,KeySort varchar(4)
        ,FilterDefinition nvarchar(max)
    )

    INSERT INTO @TableIndexColumns
    SELECT 
         IndexId                = ind.index_id
        ,IsUnique               = ind.is_unique
        ,IsConstraint           = ind.is_unique_constraint
        ,IndexType              = ind.type_desc
        ,ConstraintOrIndexName              = ind.name
        ,IndexColumnId          = ic.index_column_id
        ,IsIncludedColumn       = ic.is_included_column
        ,KeyOrdinal             = CASE WHEN ic.key_ordinal=0 THEN NULL ELSE ic.key_ordinal END
        ,ColumName              = col.name
        ,KeySort                = CASE WHEN ic.is_included_column=1 THEN NULL WHEN ic.is_descending_key=1 THEN 'DESC' ELSE 'ASC' END
        ,FilterDefinition       = ind.filter_definition
    FROM sys.tables t 
    JOIN sys.indexes ind
        ON ind.object_id = t.object_id 
    JOIN sys.index_columns ic 
        ON  ic.object_id = ind.object_id AND ic.index_id  = ind.index_id
    INNER JOIN sys.columns col 
        ON col.object_id = ic.object_id AND col.column_id = ic.column_id
    WHERE
            SCHEMA_NAME(t.schema_id) = @SchemaName AND t.name=@TableName
        AND ind.is_primary_key = 0 
        AND t.is_ms_shipped = 0 
    ORDER BY 
        ind.index_id, ic.is_included_column, ic.key_ordinal, ic.index_column_id

    -- Gather information about foreign key constraints that reference the table
    DECLARE @ForeignKeyConstraints TABLE (
         ConstraintSchema sysname
        ,ConstraintName sysname
        ,ForeignTableSchema sysname
        ,ForeignTableName sysname
        ,ForeignTableColumName sysname
        ,PrimaryTableSchema sysname
        ,PrimaryTableName sysname
        ,PrimaryTableColumnName sysname
        ,DeleteRule varchar(11)
        ,UpdateRule varchar(11)
    )

    INSERT INTO @ForeignKeyConstraints
    SELECT
         ConstraintSchema       = cn.CONSTRAINT_SCHEMA
        ,ConstraintName         = cn.CONSTRAINT_NAME
        ,ForeignTableSchema     = fk.TABLE_SCHEMA
        ,ForeignTableName       = fk.TABLE_NAME
        ,ForeignTableColumName  = cu.COLUMN_NAME
        ,PrimaryTableSchema     = pk.TABLE_SCHEMA
        ,PrimaryTableName       = pk.TABLE_NAME
        ,PrimaryTableColumnName = pt.COLUMN_NAME
        ,DeleteRule             = cn.DELETE_RULE
        ,UpdateRule             = cn.UPDATE_RULE
    FROM
        INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS cn
    JOIN INFORMATION_SCHEMA.TABLE_CONSTRAINTS fk
        ON fk.CONSTRAINT_NAME = cn.CONSTRAINT_NAME
    JOIN INFORMATION_SCHEMA.TABLE_CONSTRAINTS pk
        ON pk.CONSTRAINT_NAME = cn.UNIQUE_CONSTRAINT_NAME
    JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE cu
        ON cu.CONSTRAINT_NAME = cn.CONSTRAINT_NAME
    JOIN (
            SELECT
                 i1.TABLE_NAME
                ,i2.COLUMN_NAME
            FROM
                INFORMATION_SCHEMA.TABLE_CONSTRAINTS i1
            JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE i2
                ON i2.CONSTRAINT_NAME=i1.CONSTRAINT_NAME
            WHERE
                i1.CONSTRAINT_TYPE = 'PRIMARY KEY'
            ) pt
        ON pt.TABLE_NAME = pk.TABLE_NAME
    WHERE
           (pk.TABLE_SCHEMA=@SchemaName AND pk.TABLE_NAME=@TableName)
        OR (fk.TABLE_SCHEMA=@SchemaName AND fk.TABLE_NAME=@TableName)

    -- Gather information about the table columns
    DECLARE @TableColumns TABLE (
         ColumnName sysname
        ,PositionCurrent int
        ,PositionNew int
        ,ColumnDefault nvarchar(4000)
        ,IsNullable bit
        ,IsIdentity bit
        ,DataType nvarchar(128)
        ,CharacterMaximumLength int
        ,CharacterOctetLength int
        ,NumericPrecision tinyint
        ,NumericPrecisionRadix smallint
        ,NumericScale int
        ,DateTimePrecision smallint
        ,IsComputed bit
        ,ComputedDefinition nvarchar(max)
        ,IsPersisted bit
    )

    DECLARE @CurrentColumnNames varchar(max)
    SELECT @CurrentColumnNames = COALESCE(@CurrentColumnNames + ',', '') + CAST(cl.COLUMN_NAME AS varchar(255))
    FROM INFORMATION_SCHEMA.COLUMNS cl WHERE cl.TABLE_SCHEMA=@SchemaName AND cl.TABLE_NAME=@TableName ORDER BY cl.ORDINAL_POSITION

    -- Autofill the @ColumnNamesAsCsv parameter when its NULL. The table is not being rebuilt with a new column order.
    IF (@DropIdentityConstraint=1 OR @AddIdentityConstraint=1) AND ISNULL(@ColumnNamesAsCsv,'')=''
    BEGIN
        SET @ColumnNamesAsCsv = @CurrentColumnNames
    END

    DECLARE @ColumnNames TABLE(
         [Position] INT --IDENTITY PRIMARY KEY
        ,[Value] NVARCHAR(MAX) 
    )
    INSERT INTO @ColumnNames
    SELECT [Position],[Value] FROM [dbo].[Split](',', @ColumnNamesAsCsv)
    ORDER BY [Position]

    INSERT INTO @TableColumns
    SELECT
         ColumnName             = cl.COLUMN_NAME
        ,PositionCurrent        = cl.ORDINAL_POSITION
        ,PositionNew            = cn.Position
        ,ColumnDefault          = cl.COLUMN_DEFAULT
        ,IsNullable             = CASE WHEN cl.IS_NULLABLE='NO' THEN 0 ELSE 1 END
        ,IsIdentity             = COLUMNPROPERTY(OBJECT_ID(cl.TABLE_NAME), cl.COLUMN_NAME, 'IsIdentity')
        ,DataType               = cl.DATA_TYPE
        ,CharacterMaximumLength = cl.CHARACTER_MAXIMUM_LENGTH
        ,CharacterOctetLength   = cl.CHARACTER_OCTET_LENGTH
        ,NumericPrecision       = cl.NUMERIC_PRECISION
        ,NumericPrecisionRadix  = cl.NUMERIC_PRECISION_RADIX
        ,NumericScale           = cl.NUMERIC_SCALE
        ,DateTimePrecision      = cl.DATETIME_PRECISION
        ,IsComputed             = CASE WHEN cc.name IS NULL THEN 0 ELSE 1 END 
        ,ComputedDefinition     = CASE WHEN cc.name IS NULL THEN NULL ELSE cc.[definition] END 
        ,IsPersisted            = CASE WHEN cc.name IS NULL THEN 0 ELSE cc.[is_persisted] END 
    FROM INFORMATION_SCHEMA.COLUMNS cl
    JOIN @ColumnNames cn
        ON cn.Value = cl.COLUMN_NAME
    LEFT JOIN sys.computed_columns cc
        ON cc.name = cl.COLUMN_NAME AND cc.object_id = OBJECT_ID(@TableName)
    WHERE cl.TABLE_SCHEMA=@SchemaName AND cl.TABLE_NAME=@TableName
    ORDER BY cn.Position

    -- @PrimaryKey
    DECLARE @PrimaryKeyConstraintName sysname
    DECLARE @PrimaryKeyColumnName sysname

    SELECT
         @PrimaryKeyConstraintName = tc.CONSTRAINT_NAME
        ,@PrimaryKeyColumnName = ccu.COLUMN_NAME
    FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS tc
        JOIN INFORMATION_SCHEMA.CONSTRAINT_COLUMN_USAGE ccu ON ccu.CONSTRAINT_NAME = tc.CONSTRAINT_NAME
    WHERE tc.CONSTRAINT_TYPE = 'PRIMARY KEY'
    AND tc.TABLE_SCHEMA=@SchemaName AND tc.TABLE_NAME=@TableName
    IF @GenerateScriptOnly=0 AND @Verbosity>=2 PRINT '@PrimaryKeyConstraintName is: ' + ISNULL(CAST(@PrimaryKeyConstraintName AS varchar(255)),'NULL')
    IF @GenerateScriptOnly=0 AND @Verbosity>=2 PRINT '@PrimaryKeyColumnName is: ' + ISNULL(CAST(@PrimaryKeyColumnName AS varchar(255)),'NULL')


    -- @HasIdentityColumn
    DECLARE @HasIdentityColumn BIT
    SET @HasIdentityColumn=0
    IF EXISTS(SELECT TOP 1 1 FROM @TableColumns WHERE IsIdentity=1)
    BEGIN
        SET @HasIdentityColumn=1
    END
    IF @GenerateScriptOnly=0 AND @Verbosity>=2 PRINT '@HasIdentityColumn is: ' + ISNULL(CAST(@HasIdentityColumn AS varchar(255)),'NULL')

    DECLARE @IdentitySeed varchar(20), @IdentityIncrement varchar(20)
    SELECT @IdentitySeed=IDENT_SEED(@PrimaryTable), @IdentityIncrement=IDENT_INCR(@PrimaryTable)
    
    IF @GenerateScriptOnly=0 AND @Verbosity>=2 PRINT '@IdentitySeed is: ' + ISNULL(CAST(@IdentitySeed AS varchar(255)),'NULL')
    IF @GenerateScriptOnly=0 AND @Verbosity>=2 PRINT '@IdentityIncrement is: ' + ISNULL(CAST(@IdentityIncrement AS varchar(255)),'NULL')

    DECLARE @ColumnCountCurrent int, @ColumnCountNew int, @ColumnMatchedCount int
    SET @ColumnCountCurrent = (SELECT COUNT(*) FROM INFORMATION_SCHEMA.COLUMNS cl WHERE cl.TABLE_SCHEMA=@SchemaName AND cl.TABLE_NAME=@TableName)
    SET @ColumnCountNew = (SELECT COUNT(*) FROM @ColumnNames)
    SET @ColumnMatchedCount = (SELECT COUNT(*) FROM @TableColumns)

    IF @GenerateScriptOnly=0 AND @Verbosity>=2 PRINT '@ColumnCountCurrent is: ' + CAST(@ColumnCountCurrent AS varchar(255))
    IF @GenerateScriptOnly=0 AND @Verbosity>=2 PRINT '@ColumnCountNew is: ' + CAST(@ColumnCountNew AS varchar(255))
    IF @GenerateScriptOnly=0 AND @Verbosity>=2 PRINT '@ColumnMatchedCount is: ' + CAST(@ColumnMatchedCount AS varchar(255))


    -- Gather information about the table permissions
    DECLARE @TablePermissions TABLE (
         Username nvarchar(256)
        ,PermissionName nvarchar(128)
    )

    INSERT INTO @TablePermissions
    SELECT 
         Username = USER_NAME(dp.grantee_principal_id)
        ,PermissionName = dp.[permission_name]
    FROM
        sys.database_permissions dp
    WHERE
            OBJECT_SCHEMA_NAME(dp.major_id) = @SchemaName
        AND OBJECT_NAME(dp.major_id) = @TableName

    -- Validation
    IF     (@DropIdentityConstraint=0 AND @AddIdentityConstraint=0 AND ISNULL(@ColumnNamesAsCsv,'')='')
        OR (@ColumnCountCurrent<>@ColumnMatchedCount)
        OR (@PrimaryKeyConstraintName IS NULL) OR (@PrimaryKeyColumnName IS NULL)
    BEGIN
        IF (@DropIdentityConstraint=0 AND @AddIdentityConstraint=0 AND ISNULL(@ColumnNamesAsCsv,'')='')
        BEGIN
            RAISERROR('Error: The @ColumnNamesAsCsv parameter is required unless using @DropIdentityConstraint or @AddIdentityConstraint.', 18, 1)
        END

        ELSE IF (@ColumnCountCurrent<>@ColumnMatchedCount)
        BEGIN
            SET @ErrorMessage = 'Error: The @ColumnNamesAsCsv parameter value is invalid. Could only match ' + CAST(@ColumnMatchedCount AS varchar(255)) + ' out of the ' + CAST(@ColumnCountCurrent AS varchar(255)) + ' current columns.'
            SET @ErrorMessage = @ErrorMessage + @LF + 'Current column names: ' + @CurrentColumnNames
            RAISERROR (@ErrorMessage, 18, 1)
        END

        ELSE IF ((@PrimaryKeyConstraintName IS NULL) OR (@PrimaryKeyColumnName IS NULL))
        BEGIN
            SET @ErrorMessage = 'Error: The table must have a primary key. Note: This does not imply an IDENTITY constraint.'
            SET @ErrorMessage = @ErrorMessage + @LF + 'Current column names: ' + @CurrentColumnNames
            RAISERROR (@ErrorMessage, 18, 1)
        END
    END
    ELSE
    BEGIN
        -- Valid

        DECLARE fkcCursor CURSOR FOR
        SELECT ConstraintSchema,ConstraintName,ForeignTableSchema,ForeignTableName,ForeignTableColumName,PrimaryTableSchema,PrimaryTableName,PrimaryTableColumnName,DeleteRule,UpdateRule
        FROM @ForeignKeyConstraints

        DECLARE tcCursor CURSOR FOR
        SELECT ColumnName,PositionCurrent,PositionNew,ColumnDefault,IsNullable,IsIdentity,DataType,CharacterMaximumLength,CharacterOctetLength,NumericPrecision,NumericPrecisionRadix,NumericScale,DateTimePrecision,IsComputed,ComputedDefinition,IsPersisted
        FROM @TableColumns
        ORDER BY PositionNew

        DECLARE tiCursor CURSOR FOR
        SELECT DISTINCT IndexId,IsUnique,IsConstraint,IndexType,ConstraintOrIndexName,FilterDefinition
        FROM @TableIndexColumns

        DECLARE tpCursor CURSOR FOR
        SELECT Username,PermissionName
        FROM @TablePermissions

        DECLARE trCursor CURSOR FOR
        SELECT TriggerName,TriggerDefinition
        FROM @Triggers

        DECLARE
            -- Foreign keys
             @ConstraintSchema sysname, @ConstraintName sysname,@ForeignTableSchema sysname, @ForeignTableName sysname, @ForeignTableColumName sysname,@PrimaryTableSchema sysname, @PrimaryTableName sysname, @PrimaryTableColumnName sysname,@DeleteRule varchar(11),@UpdateRule varchar(11)
            -- Table columns
            ,@ColumnName sysname, @PositionCurrent int, @PositionNew int, @ColumnDefault nvarchar(4000),@IsNullable bit, @IsIdentity bit, @DataType nvarchar(128), @CharacterMaximumLength int, @CharacterOctetLength int,@NumericPrecision tinyint, @NumericPrecisionRadix smallint, @NumericScale int, @DateTimePrecision smallint, @IsComputed bit, @ComputedDefinition nvarchar(max), @IsPersisted bit
            -- Indexes and index columns
            ,@IndexId int,@IsUnique bit,@IsConstraint bit,@IndexType sysname,@ConstraintOrIndexName sysname,@IndexColumnId int,@IsIncludedColumn bit,@KeyOrdinal int,@ColumName sysname,@KeySort varchar(4),@FilterDefinition nvarchar(max)
            -- Permissions
            ,@Username nvarchar(256), @PermissionName nvarchar(128)
            -- Triggers
            ,@TriggerName sysname, @TriggerDefinition nvarchar(max)
            -- Miscellaneous
            ,@ColumnIndex int

        ---------------------------
        ----- Perform Changes -----
        ---------------------------

        IF @Verbosity>=1 PRINT @PrintStart + 'Rebuilding the table ' + @PrimaryTable + '...' + @PrintEnd
        DECLARE @SQL nvarchar(max)
        SET @SQL = ''

        -- Temporarily drop all foreign key constraints which reference the primary table
        IF @Verbosity>=1 PRINT ''
        IF @Verbosity>=1 PRINT @PrintStart + 'Temporarily dropping all foreign key constraints which reference the ' + @PrimaryTable + ' table...' + @PrintEnd
        OPEN fkcCursor
        FETCH NEXT FROM fkcCursor INTO @ConstraintSchema,@ConstraintName,@ForeignTableSchema,@ForeignTableName,@ForeignTableColumName,@PrimaryTableSchema,@PrimaryTableName,@PrimaryTableColumnName,@DeleteRule,@UpdateRule
        WHILE @@FETCH_STATUS = 0
        BEGIN
            SET @ForeignTable = '[' + @ForeignTableSchema + '].[' + @ForeignTableName + ']'
            SET @SQL = '
ALTER TABLE ' + @ForeignTable + ' DROP CONSTRAINT [' + @ConstraintName + ']

'
            SET @Message=''
            IF @Verbosity>=1 SET @Message=@PrintStart + 'Altering table ' + @ForeignTable + ', dropping foreign key constraint [' + @ConstraintName + ']...' + @PrintEnd
            IF @Verbosity>=2 SET @Message=@Message + @SQL
            IF ISNULL(@Message,'') <> '' PRINT @Message

            IF @GenerateScriptOnly=0 EXEC sp_executesql @Sql

            FETCH NEXT FROM fkcCursor INTO @ConstraintSchema,@ConstraintName,@ForeignTableSchema,@ForeignTableName,@ForeignTableColumName,@PrimaryTableSchema,@PrimaryTableName,@PrimaryTableColumnName,@DeleteRule,@UpdateRule
        END
        CLOSE fkcCursor

        -- Rebuild the table using the specified column order from @ColumnNamesAsCsv
        IF @Verbosity>=1 PRINT ''
        IF @Verbosity>=1 PRINT @PrintStart + 'Starting rebuild of table ' + @PrimaryTable + '...' + @PrintEnd

        DECLARE @RandomString varchar(9), @TempTableName sysname, @TempPrimaryKeyConstraintName sysname
        SET @RandomString = SUBSTRING(CONVERT(varchar(255), NEWID()), 0, 9)
        SET @TempTableName = 'tmp_' + @TableName + '_' + @RandomString
        SET @TempPrimaryKeyConstraintName   = 'tmp_PK_' + @TableName + '_' + @RandomString
        IF @GenerateScriptOnly=0 AND @Verbosity>=2 PRINT '@TempTableName is: ' + @TempTableName
        IF @GenerateScriptOnly=0 AND @Verbosity>=2 PRINT '@TempPrimaryKeyConstraintName is: ' + @TempPrimaryKeyConstraintName

        -- Transaction header
        SET @SQL = '
SET ANSI_NULLS, ANSI_PADDING, ANSI_WARNINGS, ARITHABORT, CONCAT_NULL_YIELDS_NULL, QUOTED_IDENTIFIER ON
SET NUMERIC_ROUNDABORT OFF
SET TRANSACTION ISOLATION LEVEL SERIALIZABLE
SET XACT_ABORT ON
SET NOCOUNT ON

DECLARE @TranCount int
SET @TranCount = @@TRANCOUNT

BEGIN TRY

    IF @TranCount = 0
        BEGIN TRANSACTION
    ELSE
        SAVE TRANSACTION TableRebuild

    CREATE TABLE [' + @SchemaName + '].[' + @TempTableName + '] ('

        -- Columns for the CREATE TABLE statement, specified in the column order from @ColumnNamesAsCsv.
        SET @ColumnIndex=0
        OPEN tcCursor
        FETCH NEXT FROM tcCursor INTO @ColumnName, @PositionCurrent, @PositionNew, @ColumnDefault, @IsNullable, @IsIdentity, @DataType, @CharacterMaximumLength, @CharacterOctetLength, @NumericPrecision, @NumericPrecisionRadix, @NumericScale, @DateTimePrecision, @IsComputed, @ComputedDefinition, @IsPersisted
        WHILE @@FETCH_STATUS = 0
        BEGIN
            SET @SQL=@SQL + @LF + '        '
            IF @ColumnIndex > 0 SET @SQL=@SQL + ',' ELSE SET @SQL=@SQL + ' '
        
            SET @SQL=@SQL + '[' + @ColumnName + ']'

            IF @IsComputed=1
            BEGIN
                -- Computed columns
                SET @SQL=@SQL + ' AS ' + @ComputedDefinition
                IF @IsPersisted=1 SET @SQL=@SQL + ' PERSISTED'
            END
            ELSE
            BEGIN
                -- Non-computed columns
                SET @SQL=@SQL + ' ' + @DataType
                IF @DataType='float' OR @DataType='real'
                BEGIN
                    SET @SQL=@SQL + '(' + CAST(@NumericPrecision AS varchar(50)) + ')'
                END
                IF @DataType='numeric' OR @DataType='decimal'
                BEGIN
                    SET @SQL=@SQL + '(' + CAST(@NumericPrecision AS varchar(50)) + ',' + CAST(@NumericScale AS varchar(50)) + ')'
                END
                IF ISNULL(@CharacterMaximumLength, 0) > 0 AND ISNULL(@CharacterMaximumLength, 0) <> 2147483647
                    SET @SQL=@SQL + '(' +CAST(@CharacterMaximumLength AS varchar(20)) + ')'
                IF @CharacterMaximumLength = -1
                    SET @SQL=@SQL + '(max)'

                IF (@IsIdentity=1                                        AND @DropIdentityConstraint=0) SET @SQL=@SQL + ' IDENTITY(' +      @IdentitySeed                    + ',' +      @IdentityIncrement                    + ')'
                IF (@IsIdentity=0 AND @ColumnName= @PrimaryKeyColumnName AND @AddIdentityConstraint =1) SET @SQL=@SQL + ' IDENTITY(' + CAST(@AddIdentitySeed AS VARCHAR(20)) + ',' + CAST(@AddIdentityIncrement AS VARCHAR(20)) + ')'

                IF @ColumnDefault IS NOT NULL
                BEGIN
                    -- Remove enclosing parenthesis pair
                    IF SUBSTRING(@ColumnDefault,1,1)='(' AND SUBSTRING(@ColumnDefault,LEN(@ColumnDefault),1)=')'
                    SET @ColumnDefault=SUBSTRING(@ColumnDefault,2,LEN(@ColumnDefault)-2)

                    -- Remove enclosing parenthesis pair, again
                    IF SUBSTRING(@ColumnDefault,1,1)='(' AND SUBSTRING(@ColumnDefault,LEN(@ColumnDefault),1)=')'
                    SET @ColumnDefault=SUBSTRING(@ColumnDefault,2,LEN(@ColumnDefault)-2)

                    SET @SQL=@SQL + ' DEFAULT ' + @ColumnDefault
                END

                IF @IsNullable=1 SET @SQL=@SQL + ' NULL' ELSE SET @SQL=@SQL + ' NOT NULL'

            END

            -- Column done
            SET @ColumnIndex=@ColumnIndex + 1
            FETCH NEXT FROM tcCursor INTO @ColumnName, @PositionCurrent, @PositionNew, @ColumnDefault, @IsNullable, @IsIdentity, @DataType, @CharacterMaximumLength, @CharacterOctetLength, @NumericPrecision, @NumericPrecisionRadix, @NumericScale, @DateTimePrecision, @IsComputed, @ComputedDefinition, @IsPersisted
        END
        CLOSE tcCursor

        -- Close the CREATE TABLE statement. Begin the INSERT INTO statement.
        SET @SQL=@SQL + '
        CONSTRAINT [' + @TempPrimaryKeyConstraintName + '] PRIMARY KEY CLUSTERED ([' + @PrimaryKeyColumnName + '] ASC)
    )

    IF EXISTS (SELECT TOP 1 1 FROM [' + @SchemaName + '].[' + @TableName + '])
    BEGIN
        '
        IF @Verbosity>=2 PRINT 'Line 505: @TempPrimaryKeyConstraintName is: ' + ISNULL(@TempPrimaryKeyConstraintName, 'NULL') + @LF
        IF @Verbosity>=2 PRINT 'Line 505: @PrimaryKeyColumnName is: ' + ISNULL(@PrimaryKeyColumnName, 'NULL') + @LF
        IF @Verbosity>=2 PRINT 'Line 505: @SchemaName is: ' + ISNULL(@SchemaName, 'NULL') + @LF
        IF @Verbosity>=2 PRINT 'Line 505: @TableName is: ' + ISNULL(@TableName, 'NULL') + @LF

        IF @HasIdentityColumn=1 AND @DropIdentityConstraint=0 SET @SQL= @SQL + 'SET IDENTITY_INSERT [' + @SchemaName + '].[' + @TempTableName + '] ON
        '

        IF @AddIdentityConstraint=1 SET @SQL= @SQL + 'SET IDENTITY_INSERT [' + @SchemaName + '].[' + @TempTableName + '] ON
        '

        SET @SQL= @SQL + 'INSERT INTO [' + @SchemaName + '].[' + @TempTableName + '] ('

        SET @ColumnIndex=0
        OPEN tcCursor
        FETCH NEXT FROM tcCursor INTO @ColumnName, @PositionCurrent, @PositionNew, @ColumnDefault, @IsNullable, @IsIdentity, @DataType, @CharacterMaximumLength, @CharacterOctetLength, @NumericPrecision, @NumericPrecisionRadix, @NumericScale, @DateTimePrecision, @IsComputed, @ComputedDefinition, @IsPersisted
        WHILE @@FETCH_STATUS = 0
        BEGIN
            IF @IsComputed=0
            BEGIN
                -- Non-computed columns
                SET @SQL=@SQL + @LF + '            '
                IF @ColumnIndex > 0 SET @SQL=@SQL + ',' ELSE SET @SQL=@SQL + ' '

                SET @SQL=@SQL + '[' + @ColumnName + ']'
            END

            SET @ColumnIndex=@ColumnIndex + 1
            FETCH NEXT FROM tcCursor INTO @ColumnName, @PositionCurrent, @PositionNew, @ColumnDefault, @IsNullable, @IsIdentity, @DataType, @CharacterMaximumLength, @CharacterOctetLength, @NumericPrecision, @NumericPrecisionRadix, @NumericScale, @DateTimePrecision, @IsComputed, @ComputedDefinition, @IsPersisted
        END
        CLOSE tcCursor

        -- Close the INSERT INTO statement. Begin the SELECT statement.
        SET @SQL=@SQL + '
        )
        SELECT'

        -- Columns for the SELECT statement, specified in the current column order.
        SET @ColumnIndex=0
        OPEN tcCursor
        FETCH NEXT FROM tcCursor INTO @ColumnName, @PositionCurrent, @PositionNew, @ColumnDefault, @IsNullable, @IsIdentity, @DataType, @CharacterMaximumLength, @CharacterOctetLength, @NumericPrecision, @NumericPrecisionRadix, @NumericScale, @DateTimePrecision, @IsComputed, @ComputedDefinition, @IsPersisted
        WHILE @@FETCH_STATUS = 0
        BEGIN
            IF @IsComputed=0
            BEGIN
                -- Non-computed columns
                SET @SQL=@SQL + @LF + '            '
                IF @ColumnIndex > 0 SET @SQL=@SQL + ',' ELSE SET @SQL=@SQL + ' '

                SET @SQL=@SQL + '[' + @ColumnName + ']'
            END

            SET @ColumnIndex=@ColumnIndex + 1
            FETCH NEXT FROM tcCursor INTO @ColumnName, @PositionCurrent, @PositionNew, @ColumnDefault, @IsNullable, @IsIdentity, @DataType, @CharacterMaximumLength, @CharacterOctetLength, @NumericPrecision, @NumericPrecisionRadix, @NumericScale, @DateTimePrecision, @IsComputed, @ComputedDefinition, @IsPersisted
        END
        CLOSE tcCursor
        DEALLOCATE tcCursor

        -- Transaction footer
        SET @SQL=@SQL + '
        FROM [' + @SchemaName + '].[' + @TableName + ']
        ORDER BY [' + @PrimaryKeyColumnName + '] ASC'
        IF @HasIdentityColumn=1 AND @DropIdentityConstraint=0 SET @SQL=@SQL + '
        SET IDENTITY_INSERT [' + @SchemaName + '].[' + @TempTableName + '] OFF'

        SET @SQL=@SQL + '
    END

    DROP TABLE [' + @SchemaName + '].[' + @TableName + ']
    EXECUTE sp_rename N''[' + @SchemaName + '].[' + @TempTableName + ']'', N''' + @TableName + '''
    EXECUTE sp_rename N''[' + @SchemaName + '].[' + @TempPrimaryKeyConstraintName + ']'', N''' + @PrimaryKeyConstraintName + ''', N''OBJECT''

    IF @TranCount = 0 COMMIT
    SET TRANSACTION ISOLATION LEVEL READ COMMITTED

END TRY
BEGIN CATCH

    DECLARE @XactState int
    SET @XactState = XACT_STATE()

    if @XactState = -1
        ROLLBACK
    if @XactState = 1 AND @TranCount = 0
        ROLLBACK
    if @XactState = 1 AND @TranCount > 0
        ROLLBACK TRANSACTION TableRebuild

    SET TRANSACTION ISOLATION LEVEL READ COMMITTED

    DECLARE
         @ExecuteSqlErrorSeverity int
        ,@ExecuteSqlErrorMessage nvarchar(4000)
        ,@ExecuteSqlErrorState tinyint

    SET @ExecuteSqlErrorSeverity    = ERROR_SEVERITY()
    SET @ExecuteSqlErrorMessage     = ERROR_MESSAGE()
    SET @ExecuteSqlErrorState       = ERROR_STATE()

    RAISERROR(@ExecuteSqlErrorMessage, @ExecuteSqlErrorSeverity, @ExecuteSqlErrorState)

END CATCH
'
        IF ISNULL(@SQL, '')=''  -- Something is NULL in the script.
        BEGIN
            RAISERROR('Error: The main SQL statement stored in @SQL is NULL.', 18, 1)
            GOTO Done
        END
        IF @Verbosity>=2 PRINT 'Main SQL statement:'
        IF @Verbosity>=2 EXEC [dbo].[PrintMax] @SQL

        DECLARE
             @ExecuteSqlErrorSeverity int
            ,@ExecuteSqlErrorMessage nvarchar(4000)
            ,@ExecuteSqlErrorState tinyint

        BEGIN TRY
            IF @GenerateScriptOnly=0 EXEC sp_executesql @Sql
        END TRY
        BEGIN CATCH
            SET @ExecuteSqlErrorSeverity    = ERROR_SEVERITY()
            SET @ExecuteSqlErrorMessage     = ERROR_MESSAGE()
            SET @ExecuteSqlErrorState       = ERROR_STATE()
        END CATCH

        IF @ExecuteSqlErrorState IS NOT NULL
        BEGIN
            PRINT ''
            PRINT 'An error has occurred. Recreating objects which were temporarily dropped...'
            PRINT ''
        END

        -- Add the foreign key constraints which were temporarily dropped. Even on error.
        -- First pass using WITH NOCHECK in case older data conflicts with the constraint.
        IF @Verbosity>=1 PRINT ''
        IF @Verbosity>=1 PRINT @PrintStart + 'Addding foreign key constraints which reference the ' + @PrimaryTable + ' table...' + @PrintEnd
        OPEN fkcCursor
        FETCH NEXT FROM fkcCursor INTO
                @ConstraintSchema,@ConstraintName
            ,@ForeignTableSchema,@ForeignTableName,@ForeignTableColumName
            ,@PrimaryTableSchema,@PrimaryTableName,@PrimaryTableColumnName
            ,@DeleteRule,@UpdateRule
        WHILE @@FETCH_STATUS = 0
        BEGIN
            SET @ForeignTable = '[' + @ForeignTableSchema + '].[' + @ForeignTableName + ']'
            SET @SQL = '
ALTER TABLE ' + @ForeignTable + ' WITH NOCHECK
ADD CONSTRAINT [' + @ConstraintName +'] FOREIGN KEY ([' + @ForeignTableColumName + ']) REFERENCES [' + @PrimaryTableSchema + '].[' + @PrimaryTableName + '] ([' + @PrimaryTableColumnName + ']) ON DELETE ' + @DeleteRule + ' ON UPDATE '  + @UpdateRule + '

'
            SET @Message=''
            IF @Verbosity>=1 SET @Message=@PrintStart + 'Altering table ' + @ForeignTable + ', adding foreign key constraint [' + @ConstraintName + ']...' + @PrintEnd
            IF @Verbosity>=2 SET @Message=@Message + @SQL
            IF ISNULL(@Message,'') <> '' PRINT @Message

            IF @GenerateScriptOnly=0 EXEC sp_executesql @Sql

            FETCH NEXT FROM fkcCursor INTO
                 @ConstraintSchema,@ConstraintName
                ,@ForeignTableSchema,@ForeignTableName,@ForeignTableColumName
                ,@PrimaryTableSchema,@PrimaryTableName,@PrimaryTableColumnName
                ,@DeleteRule,@UpdateRule
        END
        CLOSE fkcCursor

        -- Add the foreign key constraints that were temporarily dropped back to the table.
        -- Second pass to enable checks for future data changes
        -- http://msdn.microsoft.com/en-us/library/ms188066.aspx
        IF @Verbosity>=1 PRINT ''
        IF @Verbosity>=1 PRINT @PrintStart + 'Enabling checks for the foreign key constraints which referenced the ' + @PrimaryTable + ' table...' + @PrintEnd
        OPEN fkcCursor
        FETCH NEXT FROM fkcCursor INTO
             @ConstraintSchema,@ConstraintName
            ,@ForeignTableSchema,@ForeignTableName,@ForeignTableColumName
            ,@PrimaryTableSchema,@PrimaryTableName,@PrimaryTableColumnName
            ,@DeleteRule,@UpdateRule
        WHILE @@FETCH_STATUS = 0
        BEGIN
            SET @ForeignTable = '[' + @ForeignTableSchema + '].[' + @ForeignTableName + ']'
            SET @SQL = '
ALTER TABLE ' + @ForeignTable + ' WITH CHECK CHECK CONSTRAINT [' + @ConstraintName + ']

'

            SET @Message=''
            IF @Verbosity>=1 SET @Message=@PrintStart + 'Altering table ' + @ForeignTable + ', enabling check on foreign key constraint [' + @ConstraintName + ']...' + @PrintEnd
            IF @Verbosity>=2 SET @Message=@Message + @SQL
            IF ISNULL(@Message,'') <> '' PRINT @Message

            IF @GenerateScriptOnly=0 EXEC sp_executesql @Sql

            FETCH NEXT FROM fkcCursor INTO
                 @ConstraintSchema,@ConstraintName
                ,@ForeignTableSchema,@ForeignTableName,@ForeignTableColumName
                ,@PrimaryTableSchema,@PrimaryTableName,@PrimaryTableColumnName
                ,@DeleteRule,@UpdateRule
        END
        CLOSE fkcCursor
        DEALLOCATE fkcCursor

        IF @ExecuteSqlErrorState IS NOT NULL
        BEGIN
            -- Rethrow the error now that the temporary drops are re-added
            RAISERROR(@ExecuteSqlErrorMessage, @ExecuteSqlErrorSeverity, @ExecuteSqlErrorState)
            GOTO Done
        END

        -- Recreate all the constraints and indexes that were dropped with the table
        IF @Verbosity>=1 PRINT ''
        IF @Verbosity>=1 PRINT @PrintStart + 'Recreating constraints and indexes for the ' + @PrimaryTable + ' table...' + @PrintEnd
        OPEN tiCursor
        FETCH NEXT FROM tiCursor INTO @IndexId,@IsUnique,@IsConstraint,@IndexType,@ConstraintOrIndexName,@FilterDefinition
        WHILE @@FETCH_STATUS = 0
        BEGIN
            DECLARE @UniqueKeyword sysname = CASE WHEN @IsUnique=1 THEN ' UNIQUE' ELSE '' END
            DECLARE @ConstraintOrIndexType sysname = CASE WHEN @IsConstraint=1 THEN 'constraint' ELSE 'index' END

            IF @IsConstraint=1
            BEGIN
                -- Constraint header
                SET @SQL = '
ALTER TABLE ' + @PrimaryTable + '
ADD CONSTRAINT [' + @ConstraintOrIndexName + ']' + @UniqueKeyword + ' ' + @IndexType + ' ('
            END
            ELSE
            BEGIN
                -- Index header
                SET @SQL = '
CREATE' + @UniqueKeyword + ' ' + @IndexType + ' INDEX [' + @ConstraintOrIndexName + ']
ON ' + @PrimaryTable + ' ('
            END

            -- Columns
            DECLARE @StartedIncludeColumns bit = 0
            SET @ColumnIndex=0
            DECLARE ticCursor CURSOR FOR
            SELECT IndexId,IndexColumnId,IsIncludedColumn,KeyOrdinal,ColumName,KeySort
            FROM @TableIndexColumns
            WHERE IndexId=@IndexId
            ORDER BY IndexId, IsIncludedColumn, KeyOrdinal, IndexColumnId

            OPEN ticCursor
            FETCH NEXT FROM ticCursor INTO @IndexId,@IndexColumnId,@IsIncludedColumn,@KeyOrdinal,@ColumName,@KeySort
            WHILE @@FETCH_STATUS = 0
            BEGIN
                SET @SQL=@SQL + @LF

                -- Transition from key columns to included columns
                IF @IsIncludedColumn=1 AND @StartedIncludeColumns=0
                BEGIN
                    SET @SQL=@SQL + '    )
    INCLUDE (
'
                    SET @ColumnIndex=0
                    SET @StartedIncludeColumns=1
                END

                -- Column
                SET @SQL=@SQL + '        '
                IF @ColumnIndex > 0 SET @SQL=@SQL + ',' ELSE SET @SQL=@SQL + ' '
                SET @SQL=@SQL + '[' + @ColumName + '] '
                IF @IsIncludedColumn=0 SET @SQL=@SQL + @KeySort

                SET @ColumnIndex=@ColumnIndex + 1
                FETCH NEXT FROM ticCursor INTO @IndexId,@IndexColumnId,@IsIncludedColumn,@KeyOrdinal,@ColumName,@KeySort
            END
            CLOSE ticCursor
            DEALLOCATE ticCursor

            -- Constraint or index footer
            SET @SQL=@SQL + '
    )
'
            IF @FilterDefinition IS NOT NULL
            SET @SQL=@SQL + 'WHERE ' + @FilterDefinition + '
'

            SET @Message=''
            IF @Verbosity>=1 SET @Message=@PrintStart + 'Creating ' +  @ConstraintOrIndexType + ' [' + @ConstraintOrIndexName + '] on table ' + @PrimaryTable + @PrintEnd
            IF @Verbosity>=2 SET @Message=@Message + @SQL
            IF ISNULL(@Message,'') <> '' PRINT @Message

            IF @GenerateScriptOnly=0 EXEC sp_executesql @Sql

            FETCH NEXT FROM tiCursor INTO @IndexId,@IsUnique,@IsConstraint,@IndexType,@ConstraintOrIndexName,@FilterDefinition
        END
        CLOSE tiCursor
        DEALLOCATE tiCursor


        -- Add the table permissions which were temporarily dropped.
        IF @Verbosity>=1 PRINT ''
        IF @Verbosity>=1 PRINT @PrintStart + 'Addding permissions for the ' + @PrimaryTable + ' table...' + @PrintEnd
        OPEN tpCursor
        FETCH NEXT FROM tpCursor INTO @Username,@PermissionName
        WHILE @@FETCH_STATUS = 0
        BEGIN
            SET @Sql = 'GRANT ' + @PermissionName + ' ON ' + @PrimaryTable + ' TO [' + @Username + ']'

            SET @Message=''
            IF @Verbosity>=1 SET @Message=@PrintStart + 'Granting ' + @PermissionName + ' permission for table ' + @PrimaryTable + ' to user [' + @Username + ']...' + @PrintEnd
            IF @Verbosity>=2 SET @Message=@Message + @LF + @SQL
            IF ISNULL(@Message,'') <> '' PRINT @Message

            IF @GenerateScriptOnly=0 EXEC sp_executesql @Sql
    
            FETCH NEXT FROM tpCursor INTO @Username,@PermissionName
        END
        CLOSE tpCursor
        DEALLOCATE tpCursor

        -- Add the table triggers which were temporarily dropped.
        IF @Verbosity>=1 PRINT ''
        IF @Verbosity>=1 PRINT @PrintStart + 'Addding triggers for the ' + @PrimaryTable + ' table...' + @PrintEnd
        OPEN trCursor
        FETCH NEXT FROM trCursor INTO @TriggerName,@TriggerDefinition
        WHILE @@FETCH_STATUS = 0
        BEGIN
            SET @Sql = @TriggerDefinition

            SET @Message=''
            IF @Verbosity>=1 SET @Message=@PrintStart + 'Adding trigger [' + @TriggerName + '] to the table ' + @PrimaryTable + '...' + @PrintEnd
            IF @Verbosity>=2 SET @Message=@Message + @LF + @SQL
            IF ISNULL(@Message,'') <> '' PRINT @Message

            IF @GenerateScriptOnly=0 EXEC sp_executesql @Sql
    
            FETCH NEXT FROM trCursor INTO @TriggerName,@TriggerDefinition
        END
        CLOSE trCursor
        DEALLOCATE trCursor


        IF @Verbosity>=1 PRINT ''
        IF @Verbosity>=1 PRINT @PrintStart + 'Rebuild of the table ' + @PrimaryTable + ' has completed.' + @PrintEnd
       

    END -- Validation, is valid block
Done:
END -- Table if it exists
GO
