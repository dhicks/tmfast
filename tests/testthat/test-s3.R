test_that('make_colnames', {
    expect_equal(make_colnames(letters[1:5]),
                 c('V1', 'V2', 'V3', 'V4', 'V5'))
    expect_equal(make_colnames(letters[1:15]),
                 c('V01', 'V02', 'V03', 'V04', 'V05',
                   'V06', 'V07', 'V08', 'V09', 'V10',
                   'V11', 'V12', 'V13', 'V14', 'V15'))
})
